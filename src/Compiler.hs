{-# LANGUAGE AllowAmbiguousTypes #-}

module Compiler where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
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

makeLensesWith abbreviatedFields ''MCSEffect

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

checkDbName :: MonadThrow m => CPSFuzz (Bag r) -> m String
checkDbName (CVar x) = return x
checkDbName _ =
  throwM $ InternalError "checkDbName: compiled db terms should always be variables"

compileBinop' ::
  (MonadThrow m, FreshM m) =>
  CPSFuzz a ->
  CPSFuzz b ->
  (CPSFuzz a -> CPSFuzz b -> CPSFuzz r) ->
  m (MCSEffect r)
compileBinop' a b f = do
  MCSEffect aGraph aSink <- compile' a
  MCSEffect bGraph bSink <- compile' b
  graph <- merge aGraph bGraph
  return $ MCSEffect graph (f aSink bSink)

-- | Traces the BMCS effects of the input program. The resulting `sink` should
--  compute the same value, but without any `BMap` or `BFilter` in its syntax
--  tree. It also contains no `CShare` as an implementation detail...
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

monadSimpl :: forall a m. (Typeable a, FreshM m) => CPSFuzz a -> m (CPSFuzz a)
monadSimpl term =
  monadSimplLeft' term >>= monadSimplRight'

pureTranslateBool :: CPSFuzz Bool -> Expr Bool
pureTranslateBool = undefined

pureTranslate :: CPSFuzz Number -> Expr Number
pureTranslate = undefined

-- | Assuming the initial input db has type (Bag row), generate BMCS code for
-- computing the normalized `CPSFuzz` program.
--
-- Arguments:
-- `db`: name of the input database
-- `inScope`: the `CPSFuzz` variables that contain released information and are in scope at this point
-- `g`: the traced `EffectGraph` for the whole program
codegen' ::
  forall (row :: *) a m.
  (Typeable row, Typeable a, FreshM m) =>
  String ->
  S.Set String ->
  EffectGraph ->
  CPSFuzz (Distr a) ->
  m (BMCS (Distr a))
codegen' db inScope g (CLap c w) = do
  let fvs = fvCPSFuzz w inScope
  let sourceTerms :: M.Map String AnyExpr
      sourceTerms = M.fromList [(db, AnyExpr (EVar @row db))]
  -- 1. pull effects for each of fvs
  -- 2. fuse them all together
  -- 3. build release function using wExpr by
  --    substituting the projections for each of fvs into wExpr
  let wExpr = pureTranslate w
  undefined

monadSimplLeft' :: forall a m. (Typeable a, FreshM m) => CPSFuzz a -> m (CPSFuzz a)
monadSimplLeft' (CBind (CReturn (m :: CPSFuzz a')) f) = do
  m' <- monadSimplLeft' m
  x <- gfresh "x"
  f' <- monadSimplLeft' (f (CVar x))
  return $ substCPSFuzz x f' m'
monadSimplLeft' term = monadSimplLeft' term

isCReturn ::
  forall a b m.
  (Typeable a, Typeable b, FreshM m) =>
  (CPSFuzz a -> CPSFuzz (Distr b)) ->
  m (Maybe (a :~~: b))
isCReturn f = do
  x <- gfresh "x"
  case f (CVar x) of
    CReturn _ -> return $ eqTypeRep (typeRep @a) (typeRep @b)
    _ -> return Nothing

monadSimplRight' :: forall a m. (Typeable a, FreshM m) => CPSFuzz a -> m (CPSFuzz a)
monadSimplRight' (CBind (m :: CPSFuzz (Distr arg)) (f :: CPSFuzz arg -> CPSFuzz (Distr ret))) = do
  isReturn <- isCReturn @arg @ret f
  case isReturn of
    Just HRefl -> monadSimplRight' m
    _ -> do
      m' <- monadSimplRight' m
      x <- gfresh "x"
      f' <- monadSimplRight' (f (CVar x))
      return $ CBind m' (substCPSFuzz x f')
monadSimplRight' term = monadSimplRight' term

data AnyExpr :: * where
  AnyExpr :: Typeable r => Expr r -> AnyExpr

data SIMD f t
  = SIMD
      { _sMapFunction :: Expr (f -> t),
        _sFromName :: String,
        _sToName :: String
      }

makeLensesWith abbreviatedFields ''SIMD

data SIMDFusion f t where
  SIMD1 :: (Typeable f, Typeable t) => SIMD f t -> SIMDFusion f t
  SIMDCons ::
    (Typeable fs, Typeable ts, Typeable f, Typeable t) =>
    SIMDFusion fs ts ->
    SIMD f t ->
    SIMDFusion (fs, f) (ts, t)

simd :: Expr (f -> t) -> String -> String -> SIMD f t
simd = SIMD

fuse ::
  (Typeable fs, Typeable ts, Typeable f, Typeable t) =>
  SIMDFusion fs ts ->
  Expr (f -> t) ->
  String ->
  String ->
  SIMDFusion (fs, f) (ts, t)
fuse fusion f from to = SIMDCons fusion (simd f from to)

fusedMapFunction :: SIMDFusion f t -> Expr (f -> t)
fusedMapFunction (SIMD1 simd) = simd ^. mapFunction
fusedMapFunction (SIMDCons fusion simd) =
  let f = fusedMapFunction fusion
   in -- witness the magic of toDeepRepr!
      toDeepRepr $ \(a, b) -> (f %@ a, (simd ^. mapFunction) %@ b)

inject ::
  forall f t m.
  (Typeable f, MonadThrow m) =>
  M.Map String AnyExpr ->
  SIMDFusion f t ->
  m (Expr f)
inject inputs (SIMD1 simd) = do
  case M.lookup (simd ^. fromName) inputs of
    Just (AnyExpr (e :: Expr f')) ->
      case eqTypeRep (typeRep @f) (typeRep @f') of
        Just HRefl -> return e
        _ ->
          throwM . InternalError $
            printf
              "inject: expected input %s to have type %s, but it has type %s"
              (simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM . InternalError $ printf "inject: unknown input name %s" (simd ^. fromName)
inject inputs (SIMDCons (fusion :: SIMDFusion fs1 _) (simd :: SIMD f1 _)) = do
  acc <- inject inputs fusion
  case M.lookup (simd ^. fromName) inputs of
    Just (AnyExpr (e :: Expr f')) ->
      case eqTypeRep (typeRep @f1) (typeRep @f') of
        Just HRefl -> return $ EPair acc e
        _ ->
          throwM . InternalError $
            printf
              "inject: expected input %s to have type %s, but it has type %s"
              (simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM . InternalError $ printf "inject: unknown input name %s" (simd ^. fromName)

containsTo :: String -> SIMDFusion f t -> Bool
containsTo x (SIMD1 simd) = (simd ^. toName) == x
containsTo x (SIMDCons fusion simd) =
  containsTo x fusion || (simd ^. toName) == x

project ::
  forall f t r m.
  (Typeable t, Typeable r, MonadThrow m) =>
  String ->
  SIMDFusion f t ->
  m (Expr (t -> r))
project x (SIMD1 simd) = do
  if simd ^. toName == x
    then case eqTypeRep (typeRep @r) (typeRep @t) of
      Just HRefl -> return . ELam $ \x -> x
      _ ->
        throwM . InternalError $
          printf
            "project: expected output name %s to have type %s, but it has type %s"
            x
            (show $ typeRep @t)
            (show $ typeRep @r)
    else throwM . InternalError $ printf "project: unknown output name %s" x
project x (SIMDCons (fusion :: SIMDFusion _ ts1) (simd :: SIMD _ t1)) =
  case (containsTo x fusion, x == simd ^. toName) of
    (True, False) -> do
      projFun <- project x fusion
      return $ (projFun `ecomp` ELam EFst)
    (False, True) ->
      case eqTypeRep (typeRep @t1) (typeRep @r) of
        Just HRefl -> return (ELam ESnd)
        Nothing ->
          throwM . InternalError $
            printf
              "project: expected output %s to have type %s, but it has type %s"
              x
              (show $ typeRep @r)
              (show $ typeRep @t1)
    _ ->
      throwM . InternalError $
        printf "project: expected output %s to appear on exactly one side" x

pullMapEffectsTrans' ::
  forall (row1 :: *) (row2 :: *) m.
  (MonadThrow m, FreshM m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  String ->
  String ->
  m (Expr (row1 -> row2))
pullMapEffectsTrans' g from to =
  case M.lookup to (g ^. parents) of
    Nothing ->
      throwM . InternalError $
        printf "pullMapEffectsTrans': orphaned node %s" to
    Just p ->
      if p == from
        then pullMapEffectsStep' @row1 @row2 g from to
        else do
          case M.lookup p (g ^. types) of
            Nothing ->
              throwM . InternalError $
                printf "pullMapEffectsTrans': node %s has no type" p
            Just (SomeTypeRep (parentType :: TypeRep parentDb)) ->
              let (parentTyCon, parentRowTypes) = splitApps parentType
               in if parentTyCon == fst (splitApps (typeRep @Bag))
                    && length parentRowTypes == 1
                    then case parentRowTypes of
                      [] -> error "impossible: we just checked this list has exactly 1 element"
                      (SomeTypeRep (parentRowType :: TypeRep parentRow)) : _ ->
                        -- this makes sure parentRowType has kind *
                        case eqTypeRep (typeRepKind parentRowType) (typeRepKind (typeRep @Int)) of
                          Just HRefl ->
                            withTypeable parentRowType $ do
                              mapParent <- pullMapEffectsTrans' @row1 @parentRow g from p
                              mapTo <- pullMapEffectsTrans' @parentRow @row2 g p to
                              return $ (mapTo `ecomp` mapParent)
                          _ ->
                            throwM . InternalError $
                              printf
                                "pullMapEffectsTrans': expected parent row type %s to have kind *"
                                (show $ parentType)
                    else
                      throwM . InternalError $
                        printf "pullMapEffectsTrans': expected parent type %s to be a Bag" (show $ parentType)

pullMapEffectsStep' ::
  forall row1 row2 m.
  (MonadThrow m, FreshM m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  String ->
  String ->
  m (Expr (row1 -> row2))
pullMapEffectsStep' g from to =
  case M.lookup (from, to) (g ^. edges) of
    Nothing -> throwM $ NoSuchEdge from to
    Just (AnyEdge (e :: Edge fromDb toDb)) ->
      case ( eqTypeRep (typeRep @fromDb) (typeRep @(Bag row1)),
             eqTypeRep (typeRep @toDb) (typeRep @(Bag row2))
           ) of
        (Just HRefl, Just HRefl) ->
          case e of
            Map f -> return f
            Sum _ ->
              throwM . InternalError $
                printf "pullMapEffectsStep': expected edge (%s, %s) to be a map" from to
        _ ->
          throwM . InternalError $
            printf
              "pullMapEffectsStep': expected edge (%s, %s) to have type %s but observed %s"
              from
              to
              (show $ typeRep @(Edge (Bag row1) (Bag row2)))
              (show $ typeRep @(Edge fromDb toDb))

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError
