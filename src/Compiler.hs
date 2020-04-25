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
        _egNeighbors :: M.Map String [String],
        -- | A map from destination to its src.
        _egParents :: M.Map String String
      }

makeLensesWith abbreviatedFields ''EffectGraph

emptyEG :: EffectGraph
emptyEG = EffectGraph M.empty M.empty M.empty

-- | This is the reification of BMCS effects in a `CPSFuzz` program.
data MCSEffect r
  = MCSEffect
      { -- | A map of (src, dst) variable names, and how
        --  to compute dst from src.
        _mcsGraph :: EffectGraph,
        -- | An expression that combines variable
        --  names from `graph`, and yields the final output
        --  of the computation graph.
        _mcsSink :: CPSFuzz r
      }

data EffectGraphs
  = Sing EffectGraph
  | Bind EffectGraphs EffectGraphs

data MCSEffects r
  = MCSEffects
      { _mcsGraphs :: EffectGraphs,
        _mcsSink :: CPSFuzz r
      }

makeLensesWith abbreviatedFields ''MCSEffect

makeLensesWith abbreviatedFields ''MCSEffects

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

postprocessEffects ::
  (MonadThrow m, FreshM m) =>
  (EffectGraph -> m EffectGraph) ->
  EffectGraphs ->
  m EffectGraphs
postprocessEffects act (Sing g) = Sing <$> act g
postprocessEffects act (Bind a b) = Bind a <$> postprocessEffects act b

preprocessEffects ::
  (MonadThrow m, FreshM m) =>
  (EffectGraph -> m EffectGraph) ->
  EffectGraphs ->
  m EffectGraphs
preprocessEffects act (Sing g) = Sing <$> act g
preprocessEffects act (Bind a b) = Bind <$> preprocessEffects act a <*> pure b

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

mergeParents ::
  MonadThrow m => M.Map String String -> M.Map String String -> m (M.Map String String)
mergeParents m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM . InternalError $ printf "nodes %s have multiple parents" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

merge :: MonadThrow m => EffectGraph -> EffectGraph -> m EffectGraph
merge m1 m2 = do
  m <- mergeEdges (m1 ^. edges) (m2 ^. edges)
  p <- mergeParents (m1 ^. parents) (m2 ^. parents)
  return $ EffectGraph m (mergeNeighbors (m1 ^. neighbors) (m2 ^. neighbors)) p

coalesceEffects :: MonadThrow m => EffectGraphs -> m EffectGraph
coalesceEffects (Sing g) = return g
coalesceEffects (Bind a b) = do
  a' <- coalesceEffects a
  b' <- coalesceEffects b
  merge a' b'

coalesce :: MonadThrow m => MCSEffects r -> m (MCSEffect r)
coalesce (MCSEffects graphs r) =
  flip MCSEffect r <$> coalesceEffects graphs

insert :: MonadThrow m => EffectGraph -> Direction -> AnyEdge -> m EffectGraph
insert m dir@(from, to) e =
  case M.lookup dir (m ^. edges) of
    Nothing ->
      case M.lookup to (m ^. parents) of
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
              & parents
                %~ M.insert to from
        Just _ -> throwM . InternalError $ printf "%s already has a parent" to
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

compileBinop' :: (MonadThrow m, FreshM m) => CPSFuzz a -> CPSFuzz b -> (CPSFuzz a -> CPSFuzz b -> CPSFuzz r) -> m (MCSEffects r)
compileBinop' a b f = do
  a' <- compile' a
  b' <- compile' b
  MCSEffect aGraph aSink <- coalesce a'
  MCSEffect bGraph bSink <- coalesce b'
  graph <- merge aGraph bGraph
  return $ MCSEffects (Sing graph) (f aSink bSink)

compile' :: (MonadThrow m, FreshM m) => CPSFuzz r -> m (MCSEffects r)
compile' (CVar x) = return $ MCSEffects (Sing emptyEG) (CVar x)
compile' (CNumLit n) = return $ MCSEffects (Sing emptyEG) (CNumLit n)
compile' (CAdd a b) = compileBinop' a b (+)
compile' (CMinus a b) = compileBinop' a b (-)
compile' (CMult a b) = compileBinop' a b (*)
compile' (CDiv a b) = compileBinop' a b (/)
compile' (CAbs a) = do
  MCSEffects aGraph aSink <- compile' a
  return $ MCSEffects aGraph (abs aSink)
compile' (CGT a b) = compileBinop' a b (%>)
compile' (CGE a b) = compileBinop' a b (%>=)
compile' (CLT a b) = compileBinop' a b (%<)
compile' (CLE a b) = compileBinop' a b (%<=)
compile' (CEQ a b) = compileBinop' a b (%==)
compile' (CNEQ a b) = compileBinop' a b (%/=)
compile' (BMap f db kont) = do
  db' <- compile' db
  MCSEffect dbGraph dbSink <- coalesce db'
  srcName <- checkDbName dbSink
  tgtName <- gfresh "bmap_result"
  MCSEffects kontGraphs kontSink <- compile' (kont (CVar tgtName))
  graphs <- flip preprocessEffects kontGraphs $ \g -> do
    g' <- merge dbGraph g
    insert g' (srcName, tgtName) (AnyEdge (Map f))
  return $ MCSEffects graphs kontSink
compile' (BSum clip (db :: CPSFuzz (Bag row)) kont) = do
  db' <- compile' db
  MCSEffect dbGraph dbSink <- coalesce db'
  srcName <- checkDbName dbSink
  tgtName <- gfresh "bsum_result"
  MCSEffects kontGraphs kontSink <- compile' (kont (CVar tgtName))
  graphs <- flip preprocessEffects kontGraphs $ \g -> do
    g' <- merge dbGraph g
    insert g' (srcName, tgtName) (AnyEdge @(Bag row) @row (Sum clip))
  return $ MCSEffects graphs kontSink
compile' (CShare v f) = do
  v' <- compile' v
  MCSEffect vGraph vSink <- coalesce v'
  MCSEffects fGraphs fSink <- compile' (f vSink)
  graphs <- flip preprocessEffects fGraphs (merge vGraph)
  return $ MCSEffects graphs fSink
compile' (CReturn m) = do
  MCSEffects mGraphs mSink <- compile' m
  return $ MCSEffects mGraphs (CReturn mSink)
compile' (CBind (m :: CPSFuzz (Distr a)) f) = do
  MCSEffects mGraphs mSink <- compile' m
  mName <- gfresh "cbind_m"
  MCSEffects fGraphs fSink <- compile' (f (CVar mName))
  let fSinkFun = \(bound :: CPSFuzz a) -> substCPSFuzz mName fSink bound
  return $ MCSEffects (Bind mGraphs fGraphs) (CBind mSink fSinkFun)
compile' (CLap w c) = do
  c' <- compile' c
  MCSEffect cGraph cSink <- coalesce c'
  return $ MCSEffects (Sing cGraph) (CLap w cSink)

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError
