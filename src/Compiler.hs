module Compiler where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Proxy
import IfCxt
import Lib
import Names
import Text.Printf
import Type.Reflection

data Edge (from :: *) (to :: *) where
  Map :: Expr (from -> to) -> Edge (Bag from) (Bag to)
  Sum :: Number -> Edge (Bag sum) sum

data AnyEdge :: * where
  AnyEdge ::
    (Typeable from, Typeable to) =>
    Edge from to ->
    AnyEdge

-- | A directed edge.
type Direction = (String, String)

-- | A DAG that holds the structure of the effects.
data EffectGraph
  = EffectGraph
      { -- | The computation edges from some
        --  `src` variable to `dst`
        --  variable.
        _egEdges :: M.Map Direction AnyEdge,
        -- | All destinations of any given
        --  `src`
        _egNeighbors :: M.Map String [String]
      }

makeLensesWith abbreviatedFields ''EffectGraph

emptyEG :: EffectGraph
emptyEG = EffectGraph M.empty M.empty

-- | This is the reification of BMCS effects in a `CPSFuzz` program.
data MCSEffect r
  = MCSEffect
      { -- | A map of (src, dst) variable names, and how
        --  to compute dst from src.
        _hcGraph :: EffectGraph,
        -- | An expression that combines variable
        --  names from `graph`, and yields the final output
        --  of the computation graph.
        _hcSink :: CPSFuzz r
      }

makeLensesWith abbreviatedFields ''MCSEffect

data CompilerError
  = InternalError String
  | -- | We need a type that satisfies `VecMonoid`
    --  but instead got this.
    RequiresVecMonoid SomeTypeRep
  | -- | We need a type that satisfies `Clip`, but
    --  instead got this.
    RequiresClip SomeTypeRep
  | TypeError {expected :: SomeTypeRep, observed :: SomeTypeRep}
  deriving (Show, Eq, Ord, Typeable)

newtype Compiler a = Compiler {runCompiler_ :: StateT NameState (Either SomeException) a}
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadState NameState)
    via (StateT NameState (Either SomeException))

runCompiler :: Compiler a -> Either SomeException a
runCompiler = flip evalStateT emptyNameState . runCompiler_

-- | Takes the disjoint union of two maps.
mergeEdges ::
  MonadThrow m =>
  M.Map Direction AnyEdge ->
  M.Map Direction AnyEdge ->
  m (M.Map Direction AnyEdge)
mergeEdges m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM . InternalError $ printf "directions %s are duplicated" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

-- | Simply takes the union of neighbors from both sides.
mergeNeighbors :: M.Map String [String] -> M.Map String [String] -> M.Map String [String]
mergeNeighbors = M.unionWith (++)

merge :: MonadThrow m => EffectGraph -> EffectGraph -> m EffectGraph
merge m1 m2 = do
  m <- mergeEdges (m1 ^. edges) (m2 ^. edges)
  return $ EffectGraph m (mergeNeighbors (m1 ^. neighbors) (m2 ^. neighbors))

insert :: MonadThrow m => EffectGraph -> Direction -> AnyEdge -> m EffectGraph
insert m dir@(from, to) e =
  case M.lookup dir (m ^. edges) of
    Nothing ->
      return $
        m & edges %~ M.insert dir e
          & neighbors
            %~ M.alter
              ( \case
                  Nothing -> Just [to]
                  Just ns -> Just (to : ns)
              )
              from
    Just _ -> throwM . InternalError $ printf "direction %s is duplicated" (show dir)

isPure :: forall r. CPSFuzz r -> Bool
isPure (CVar _) =
  fst (splitApps (typeRep @r)) /= fst (splitApps (typeRep @Distr))
isPure (BMap _ _ kont) = isPure (kont (CVar secretVarName))
isPure (BSum _ _ kont) = isPure (kont (CVar secretVarName))
isPure (CShare v f) = isPure v && isPure (f (CVar secretVarName))
isPure (CReturn _) = False
isPure (CBind _ _) = False
isPure (CLap _ _) = False
isPure _ = True

checkDbName :: MonadThrow m => CPSFuzz (Bag r) -> m String
checkDbName (CVar x) = return x
checkDbName _ =
  throwM $ InternalError "checkDbName: compiled db terms should always be variables"

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError
