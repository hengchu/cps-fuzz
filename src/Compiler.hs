{-# LANGUAGE AllowAmbiguousTypes #-}

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
        _egParents :: M.Map String String,
        -- | A map from variable name to its type.
        _egTypes :: M.Map String SomeTypeRep
      }

makeLensesWith abbreviatedFields ''EffectGraph

emptyEG :: EffectGraph
emptyEG = EffectGraph M.empty M.empty M.empty M.empty

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

{-
data EffectGraphs
  = Sing EffectGraph
  | Bind EffectGraphs EffectGraphs

{-# COMPLETE ConsGraph, Sing #-}
{-# COMPLETE SnocGraph, Sing #-}

pattern ConsGraph :: EffectGraph -> EffectGraphs -> EffectGraphs
pattern ConsGraph x xs <- (viewl -> Just (x, xs))

pattern SnocGraph :: EffectGraphs -> EffectGraph -> EffectGraphs
pattern SnocGraph xs x <- (viewr -> Just (xs, x))

viewl :: EffectGraphs -> Maybe (EffectGraph, EffectGraphs)
viewl (Sing _) = Nothing
viewl (Bind (Sing l) r) = Just (l, r)
viewl (Bind l r) = do
  (head, tail) <- viewl l
  Just (head, Bind tail r)

viewr :: EffectGraphs -> Maybe (EffectGraphs, EffectGraph)
viewr (Sing _) = Nothing
viewr (Bind l (Sing r)) = Just (l, r)
viewr (Bind l r) = do
  (front, last) <- viewr r
  Just (Bind l front, last)

data MCSEffects r
  = MCSEffects
      { _mcsGraphs :: EffectGraphs,
        _mcsSink :: CPSFuzz r
      }
-}

makeLensesWith abbreviatedFields ''MCSEffect

-- makeLensesWith abbreviatedFields ''MCSEffects

data CompilerError
  = InternalError String
  | -- | We need a type that satisfies `VecMonoid`
    --  but instead got this.
    RequiresVecMonoid SomeTypeRep
  | -- | We need a type that satisfies `Clip`, but
    --  instead got this.
    RequiresClip SomeTypeRep
  | NoSuchEdge {from :: String, to :: String}
  | TypeError {expected :: SomeTypeRep, observed :: SomeTypeRep}
  deriving (Show, Eq, Ord, Typeable)

newtype Compiler a = Compiler {runCompiler_ :: StateT NameState (Either SomeException) a}
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadState NameState)
    via (StateT NameState (Either SomeException))

runCompiler :: Compiler a -> Either SomeException a
runCompiler = flip evalStateT emptyNameState . runCompiler_

{-
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
-}

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

mergeTypes ::
  MonadThrow m => M.Map String SomeTypeRep -> M.Map String SomeTypeRep -> m (M.Map String SomeTypeRep)
mergeTypes m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM . InternalError $ printf "nodes %s have multiple types" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

merge :: MonadThrow m => EffectGraph -> EffectGraph -> m EffectGraph
merge m1 m2 = do
  m <- mergeEdges (m1 ^. edges) (m2 ^. edges)
  p <- mergeParents (m1 ^. parents) (m2 ^. parents)
  tys <- mergeTypes (m1 ^. types) (m2 ^. types)
  return $ EffectGraph m (mergeNeighbors (m1 ^. neighbors) (m2 ^. neighbors)) p tys

{-
coalesceEffects :: MonadThrow m => EffectGraphs -> m EffectGraph
coalesceEffects (Sing g) = return g
coalesceEffects (Bind a b) = do
  a' <- coalesceEffects a
  b' <- coalesceEffects b
  merge a' b'

coalesce :: MonadThrow m => MCSEffects r -> m (MCSEffect r)
coalesce (MCSEffects graphs r) =
  flip MCSEffect r <$> coalesceEffects graphs
-}

insert :: forall toType m. (MonadThrow m, Typeable toType) => EffectGraph -> Direction -> AnyEdge -> m EffectGraph
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
              & types
                %~ M.insert to (SomeTypeRep (typeRep @toType))
        Just _ -> throwM . InternalError $ printf "%s already has a parent" to
    Just _ -> throwM . InternalError $ printf "direction %s is duplicated" (show dir)

{-
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
-}

checkDbName :: MonadThrow m => CPSFuzz (Bag r) -> m String
checkDbName (CVar x) = return x
checkDbName _ =
  throwM $ InternalError "checkDbName: compiled db terms should always be variables"

compileBinop' ::
  (MonadThrow m, FreshM m)
  => CPSFuzz a
  -> CPSFuzz b
  -> (CPSFuzz a -> CPSFuzz b -> CPSFuzz r)
  -> m (MCSEffect r)
compileBinop' a b f = do
  MCSEffect aGraph aSink <- compile' a
  MCSEffect bGraph bSink <- compile' b
  graph <- merge aGraph bGraph
  return $ MCSEffect graph (f aSink bSink)

compile' :: (MonadThrow m, FreshM m) => CPSFuzz r -> m (MCSEffect r)
compile' (CVar x) = return $ MCSEffect emptyEG (CVar x)
compile' (CNumLit n) = return $ MCSEffect emptyEG (CNumLit n)
compile' (CAdd a b) = compileBinop' a b (+)
compile' (CMinus a b) = compileBinop' a b (-)
compile' (CMult a b) = compileBinop' a b (*)
compile' (CDiv a b) = compileBinop' a b (/)
compile' (CAbs a) = do
  MCSEffect aGraph aSink <- compile' a
  return $ MCSEffect aGraph (abs aSink)
compile' (CGT a b) = compileBinop' a b (%>)
compile' (CGE a b) = compileBinop' a b (%>=)
compile' (CLT a b) = compileBinop' a b (%<)
compile' (CLE a b) = compileBinop' a b (%<=)
compile' (CEQ a b) = compileBinop' a b (%==)
compile' (CNEQ a b) = compileBinop' a b (%/=)
compile' (BMap (f :: Expr (row1 -> row2)) db kont) = do
  MCSEffect dbGraph dbSink <- compile' db
  srcName <- checkDbName dbSink
  tgtName <- gfresh "bmap_result"
  MCSEffect kontGraph kontSink <- compile' (kont (CVar tgtName))
  graphs <- merge dbGraph kontGraph >>= \g' ->
    insert @(Bag row2) g' (srcName, tgtName) (AnyEdge (Map f))
  return $ MCSEffect graphs kontSink
compile' (BSum clip (db :: CPSFuzz (Bag row)) kont) = do
  MCSEffect dbGraph dbSink <- compile' db
  srcName <- checkDbName dbSink
  tgtName <- gfresh "bsum_result"
  MCSEffect kontGraph kontSink <- compile' (kont (CVar tgtName))
  graphs <- merge dbGraph kontGraph >>= \g' ->
    insert @row g' (srcName, tgtName) (AnyEdge @(Bag row) @row (Sum clip))
  return $ MCSEffect graphs kontSink
compile' (CShare v f) = do
  MCSEffect vGraph vSink <- compile' v
  MCSEffect fGraph fSink <- compile' (f vSink)
  graph <- merge vGraph fGraph
  return $ MCSEffect graph fSink
compile' (CReturn m) = do
  MCSEffect mGraph mSink <- compile' m
  return $ MCSEffect mGraph (CReturn mSink)
compile' (CBind (m :: CPSFuzz (Distr a)) f) = do
  MCSEffect mGraph mSink <- compile' m
  mName <- gfresh "cbind_m"
  MCSEffect fGraph fSink <- compile' (f (CVar mName))
  let fSinkFun = \(bound :: CPSFuzz a) -> substCPSFuzz mName fSink bound
  graph <- merge mGraph fGraph
  return $ MCSEffect graph (CBind mSink fSinkFun)
compile' (CLap w c) = do
  MCSEffect cGraph cSink <- compile' c
  return $ MCSEffect cGraph (CLap w cSink)

pullMapEffectsTrans' ::
  forall (row1 :: *) (row2 :: *) m.
  (MonadThrow m, FreshM m, Typeable row1, Typeable row2)
  => EffectGraph -> String -> String -> m (Expr (row1 -> row2))
pullMapEffectsTrans' g from to =
  case M.lookup to (g ^. parents) of
    Nothing -> throwM . InternalError $
      printf "pullMapEffectsTrans': orphaned node %s" to
    Just p -> if p == from
              then pullMapEffectsStep' @row1 @row2 g from to
              else do
      case M.lookup p (g ^. types) of
        Nothing -> throwM . InternalError $
          printf "pullMapEffectsTrans': node %s has no type" p
        Just (SomeTypeRep (parentType :: TypeRep parentDb)) ->
          let (parentTyCon, parentRowTypes) = splitApps parentType
          in if parentTyCon == fst (splitApps (typeRep @Bag))
                && length parentRowTypes == 1
             then case parentRowTypes of
                    [] -> error "impossible: we just checked this list has exactly 1 element"
                    (SomeTypeRep (parentRowType :: TypeRep parentRow)):_ ->
                      -- this makes sure parentRowType has kind *
                      case eqTypeRep (typeRepKind parentRowType) (typeRepKind (typeRep @Int)) of
                        Just HRefl ->
                          withTypeable parentRowType $ do
                          mapParent <- pullMapEffectsTrans' @row1 @parentRow g from p
                          mapTo <- pullMapEffectsTrans' @parentRow @row2 g p to
                          return $ (mapTo `ecomp` mapParent)
                        _ -> throwM . InternalError $
                          printf "pullMapEffectsTrans': expected parent row type %s to have kind *"
                          (show $ parentType)
             else throwM . InternalError $
                  printf "pullMapEffectsTrans': expected parent type %s to be a Bag" (show $ parentType)

pullMapEffectsStep' ::
  forall row1 row2 m. (MonadThrow m, FreshM m, Typeable row1, Typeable row2)
  => EffectGraph -> String -> String -> m (Expr (row1 -> row2))
pullMapEffectsStep' g from to =
  case M.lookup (from, to) (g ^. edges) of
    Nothing -> throwM $ NoSuchEdge from to
    Just (AnyEdge (e :: Edge fromDb toDb)) ->
      case ( eqTypeRep (typeRep @fromDb) (typeRep @(Bag row1))
           , eqTypeRep (typeRep @toDb) (typeRep @(Bag row2))
           ) of
        (Just HRefl, Just HRefl) ->
          case e of
            Map f -> return f
            Sum _ -> throwM . InternalError
              $ printf "pullMapEffectsStep': expected edge (%s, %s) to be a map" from to
        _ -> throwM . InternalError
          $ printf "pullMapEffectsStep': expected edge (%s, %s) to have type %s but observed %s"
          from to (show $ typeRep @(Edge (Bag row1) (Bag row2))) (show $ typeRep @(Edge fromDb toDb))

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError
