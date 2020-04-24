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
      { _egEdges :: M.Map Direction AnyEdge,
        _egNeighbors :: M.Map String [String]
      }

makeLensesWith abbreviatedFields ''EffectGraph

emptyEG :: EffectGraph
emptyEG = EffectGraph M.empty M.empty

data Compiled r
  = Compiled
      { _cGraph :: EffectGraph,
        _cSink :: Expr r
      }

data AlmostCompiled r
  = AlmostCompiled
      { _hcGraph :: EffectGraph,
        _hcSink :: CPSFuzz r
      }

pureAlmostCompiled :: CPSFuzz r -> AlmostCompiled r
pureAlmostCompiled = AlmostCompiled emptyEG

data CompiledChain (r :: *) where
  CSing :: Compiled r -> CompiledChain r
  CCons :: Compiled a -> CompiledChain (a -> b) -> CompiledChain b
  CSnoc :: CompiledChain a -> Compiled (a -> b) -> CompiledChain b

data AlmostCompiledChain (r :: *) where
  ACSing :: AlmostCompiled r -> AlmostCompiledChain r
  ACCons :: AlmostCompiled a -> AlmostCompiledChain (a -> b) -> AlmostCompiledChain b
  ACSnoc :: AlmostCompiledChain a -> AlmostCompiled (a -> b) -> AlmostCompiledChain b

makeLensesWith abbreviatedFields ''Compiled
makeLensesWith abbreviatedFields ''AlmostCompiled

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

compileBinop' ::
  (MonadThrow m, FreshM m) =>
  CPSFuzz a ->
  CPSFuzz b ->
  (CPSFuzz a -> CPSFuzz b -> CPSFuzz r) ->
  m (AlmostCompiled r)
compileBinop' a b f = do
  a' <- compilePure' a
  b' <- compilePure' b
  g <- merge (a' ^. graph) (b' ^. graph)
  return $ AlmostCompiled g (f (a' ^. sink) (b' ^. sink))

-- |Compiles non-monadic (but potentially has BMCS effects) `CPSFuzz` into a DAG.
compilePure' :: (MonadThrow m, FreshM m) => CPSFuzz r -> m (AlmostCompiled r)
compilePure' = undefined

-- |Compiles monadic `CPSFuzz` into a chain of DAGs.
compileMonadic' :: (MonadThrow m, FreshM m) => CPSFuzz (Distr r) -> m (AlmostCompiledChain r)
compileMonadic' = undefined

-- |Top-level function that compiles a whole `CPSFuzz` program into a DAG chain.
compile' :: (MonadThrow m, FreshM m) => CPSFuzz r -> m (AlmostCompiledChain r)
compile' = undefined

checkDbName :: MonadThrow m => CPSFuzz (Bag r) -> m String
checkDbName (CVar x) = return x
checkDbName _ = throwM $ InternalError "checkDbName: compiled db terms should always be variables"

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError
