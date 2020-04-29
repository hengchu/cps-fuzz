{-# LANGUAGE AllowAmbiguousTypes #-}

module Compiler where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Constraint
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
import GHC.Stack
import Lib
import Names
import Text.Printf
import Type.Reflection
import Debug.Trace
import Pretty

data Edge (from :: *) (to :: *) where
  Map :: Expr (from -> to) -> Edge (Bag from) (Bag to)
  Sum :: Vec Number -> Edge (Bag sum) sum

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

data SIMD f t
  = SIMD
      { _sMapFunction :: Expr (f -> t),
        _sFromName :: String,
        _sToName :: String
      }

makeLensesWith abbreviatedFields ''SIMD

data CompilerError
  = -- | An internal error, this means a bug in the compiler.
    InternalError String
  | -- | We need a type that satisfies `VecMonoid`
    --  but instead got this.
    RequiresVecMonoid SomeTypeRep
  | -- | We need a type that satisfies `Clip`, but
    --  instead got this.
    RequiresClip SomeTypeRep
  | -- | Cannot find the computation step for computing `from` to `to.
    NoSuchEdge {from :: String, to :: String}
  | -- | A typecheck error.
    TypeError {expected :: SomeTypeRep, observed :: SomeTypeRep}
  | -- | The clip bound has incorrect representation size.
    ClipSizeError {expectedSize :: Int, observedSize :: Int}
  | -- | Code seems to be releasing private information
    ReleasesPrivateData [String]
  deriving (Show, Eq, Ord, Typeable)

data WithCallStack e
  = WithCallStack
      { _wcsException :: e,
        _wcsStack :: CallStack
      }
  deriving (Typeable)

makeLensesWith abbreviatedFields ''WithCallStack

type MonadThrowWithStack m = (MonadThrow m, HasCallStack)

newtype Compiler a = Compiler {runCompiler_ :: StateT NameState (Either SomeException) a}
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadState NameState)
    via (StateT NameState (Either SomeException))

runCompiler :: Compiler a -> Either SomeException a
runCompiler = flip evalStateT emptyNameState . runCompiler_

throwM_ :: (HasCallStack, Exception e, MonadThrowWithStack m) => e -> m a
throwM_ e = throwM (WithCallStack e callStack)

-- | Takes the disjoint union of two maps.
mergeEdges ::
  MonadThrowWithStack m =>
  M.Map Direction AnyEdge ->
  M.Map Direction AnyEdge ->
  m (M.Map Direction AnyEdge)
mergeEdges m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM_ . InternalError $ printf "directions %s are duplicated" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

-- | Simply takes the union of neighbors from both sides.
mergeNeighbors :: M.Map String [String] -> M.Map String [String] -> M.Map String [String]
mergeNeighbors = M.unionWith (++)

mergeParents ::
  MonadThrowWithStack m => M.Map String String -> M.Map String String -> m (M.Map String String)
mergeParents m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM_ . InternalError $ printf "nodes %s have multiple parents" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

mergeTypes ::
  MonadThrowWithStack m => M.Map String SomeTypeRep -> M.Map String SomeTypeRep -> m (M.Map String SomeTypeRep)
mergeTypes m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM_ . InternalError $ printf "nodes %s have multiple types" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

merge :: MonadThrowWithStack m => EffectGraph -> EffectGraph -> m EffectGraph
merge m1 m2 = do
  m <- mergeEdges (m1 ^. edges) (m2 ^. edges)
  p <- mergeParents (m1 ^. parents) (m2 ^. parents)
  tys <- mergeTypes (m1 ^. types) (m2 ^. types)
  return $ EffectGraph m (mergeNeighbors (m1 ^. neighbors) (m2 ^. neighbors)) p tys

insert :: forall toType m. (MonadThrowWithStack m, Typeable toType) => EffectGraph -> Direction -> AnyEdge -> m EffectGraph
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
        Just _ -> throwM_ . InternalError $ printf "%s already has a parent" to
    Just _ -> throwM_ . InternalError $ printf "direction %s is duplicated" (show dir)

checkDbName :: MonadThrowWithStack m => CPSFuzz (Bag r) -> m String
checkDbName (CVar x) = return x
checkDbName _ =
  throwM_ $ InternalError "checkDbName: compiled db terms should always be variables"

compileBinop' ::
  (MonadThrowWithStack m, FreshM m) =>
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
compile' :: (MonadThrowWithStack m, FreshM m) => CPSFuzz r -> m (MCSEffect r)
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

pureTranslate :: CPSFuzz a -> Expr a
pureTranslate (CVar x) = EVar x
pureTranslate (CNumLit x) = ENumLit x
pureTranslate (CAdd a b) = EAdd (pureTranslate a) (pureTranslate b)
pureTranslate (CMinus a b) = EMinus (pureTranslate a) (pureTranslate b)
pureTranslate (CMult a b) = EMult (pureTranslate a) (pureTranslate b)
pureTranslate (CDiv a b) = EDiv (pureTranslate a) (pureTranslate b)
pureTranslate (CAbs a) = EAbs (pureTranslate a)
pureTranslate (CGT a b) = EGT (pureTranslate a) (pureTranslate b)
pureTranslate (CGE a b) = EGE (pureTranslate a) (pureTranslate b)
pureTranslate (CLT a b) = ELT (pureTranslate a) (pureTranslate b)
pureTranslate (CLE a b) = ELE (pureTranslate a) (pureTranslate b)
pureTranslate (CEQ a b) = EEQ (pureTranslate a) (pureTranslate b)
pureTranslate (CNEQ a b) = ENEQ (pureTranslate a) (pureTranslate b)
pureTranslate _ = error "pureTranslate: impure fragment"

compile ::
  forall row a m.
  (FreshM m, MonadThrowWithStack m, Typeable row, Typeable a) =>
  String ->
  (CPSFuzz (Bag row) -> CPSFuzz (Distr a)) ->
  m (BMCS (Distr a))
compile db prog = do
  effects <- compile' (prog (CVar db))
  simplifiedSink <- monadSimpl (effects ^. sink)
  codegen' @row db S.empty (effects ^. graph) simplifiedSink

-- | Assuming the initial input db has type (Bag row), generate BMCS code for
-- computing the normalized `CPSFuzz` program.
--
-- Arguments:
-- `db`: name of the input database
-- `inScope`: the `CPSFuzz` variables that contain released information and are in scope at this point
-- `g`: the traced `EffectGraph` for the whole program
codegen' ::
  forall (row :: *) a m.
  (Typeable row, Typeable a, FreshM m, MonadThrowWithStack m) =>
  String ->
  S.Set String ->
  EffectGraph ->
  CPSFuzz (Distr a) ->
  m (BMCS (Distr a))
codegen' db inScope g (CBind m f) = do
  m' <- codegen' @row db inScope g m
  x <- gfresh "x"
  f' <- codegen' @row db (S.insert x inScope) g (f (CVar x))
  return $ BBind m' (substBMCS x f')
codegen' _db inScope _ (CReturn m) = do
  let fvs = S.toList (fvCPSFuzz m inScope)
  case fvs of
    [] -> do
      return (BReturn (Green $ pureTranslate m))
    _ -> throwM_ . ReleasesPrivateData $ fvs
codegen' db inScope g (CLap c w) = do
  let fvs = S.toList (fvCPSFuzz w inScope)
  fvTypes <- traverse (getType $ g ^. types) fvs
  let fvWithTypes = nub (zip fvs fvTypes)
  let sourceTerms :: M.Map String AnyExpr
      sourceTerms = M.fromList [(db, AnyExpr (EVar @row db))]
  -- 1. pull effects for each of fvs
  -- 2. fuse them all together
  -- 3. build release function using wExpr by
  --    substituting the projections for each of fvs into wExpr
  let wExpr = pureTranslate w
  fvParents <- traverse (getParent $ g ^. parents) fvs
  let fvWithTypesWithParent = nub $ zip (zip fvs fvTypes) fvParents
  parentTypes <- traverse (getType $ g ^. types) fvParents
  let parentWithTypes = nub (zip fvParents parentTypes)
  case fvs of
    [] -> return (Green (ELap c wExpr))
    _ -> codegenFusedMap' @row db parentWithTypes g $
      \(fusion :: SIMDFusion fs ts) -> do
        let mf :: (Expr (fs -> ts)) = fusedMapFunction fusion
        (fusedInput :: Expr fs) <- inject sourceTerms fusion
        let inj :: (Expr (row -> fs)) = toDeepRepr $
              \(input :: Expr row) -> substExpr db fusedInput input
        delayedSubsts <- substWithProjection fusion fvWithTypesWithParent
        let releaseFun =
              \(releaseTerm :: Expr ts) ->
                let runDelayedSubsts t [] = t
                    runDelayedSubsts t ((x, proj):more) =
                      --trace x $
                      case proj releaseTerm of
                        AnyExpr needle ->
                          --traceShow (runP $ prettyExpr 0 needle) $
                          runDelayedSubsts (substExpr x t needle) more
                in ELap c (runDelayedSubsts wExpr delayedSubsts)
        case resolveClip @ts of
          Nothing -> throwM_ $ RequiresClip (SomeTypeRep (typeRep @ts))
          Just dict ->
            withDict dict
              $ return
              $ Run (vecSize @ts) (Vec [0]) (mf `ecomp` inj) (toDeepRepr releaseFun)
  where
    getParent parents x =
      case M.lookup x parents of
        Just p -> return p
        Nothing ->
          throwM_ . InternalError $
            printf "codegen': %s has no parent" x
    getType types x =
      case M.lookup x types of
        Just ty -> return ty
        Nothing ->
          throwM_ . InternalError $
            printf "codegen': %s has no known type" x
    substWithProjection ::
      forall fs ts.
      (Typeable fs, Typeable ts) =>
      SIMDFusion fs ts ->
      [((String, SomeTypeRep), String)] ->
      m [(String, Expr ts -> AnyExpr)]
    substWithProjection _      []                       = return []
    substWithProjection fusion (((x, xTR), xParent) : xs) =
      case xTR of
        SomeTypeRep (xDbType :: TypeRep xRowType) -> do
          withTypeable xDbType $
            withKindStar @_ @xRowType $ do
              projX <- project @_ @ts @xRowType xParent fusion
              more <- substWithProjection fusion xs
              return $ (x, \(summed :: Expr ts) -> AnyExpr (projX %@ summed)):more

codegen' _ _ _ _ = throwM_ . InternalError $ "codegen': unexpected CPSFuzz term"

codegenFusedMap' ::
  forall (row :: *) r m.
  (Typeable row, FreshM m, MonadThrowWithStack m) =>
  String ->
  [(String, SomeTypeRep)] ->
  EffectGraph ->
  (forall fs ts. (Typeable fs, Typeable ts) => SIMDFusion fs ts -> m r) ->
  m r
codegenFusedMap' _db [] _g _kont =
  throwM_ . InternalError $ "codegenFusedMap': expecting at least 1 free variable"
codegenFusedMap' db [(x, xTR)] g kont =
  case xTR of
    SomeTypeRep (xDbType :: TypeRep xDbType) ->
      withTypeable xDbType
        $ withKindStar @_ @xDbType
        $ withTypeBag @xDbType
        $ \(_ :: Proxy xRowType) ->
          withKindStar @_ @xDbType $ do
            f <- pullMapEffectsTrans' @row @xRowType g db x
            kont (SIMD1 $ simd f db x)
codegenFusedMap' db ((x, xTR) : xs) g kont = do
  codegenFusedMap' @row db [(x, xTR)] g $
    \case
      SIMD1 (SIMD f from to) ->
        codegenFusedMap' @row db xs g $
          \fusionXs -> kont (fuse fusionXs f from to)
      _ -> throwM_ . InternalError $ "codegenFusedMap': expected SIMD1 here"

monadSimplLeft' :: forall a m. (Typeable a, FreshM m) => CPSFuzz a -> m (CPSFuzz a)
monadSimplLeft' (CBind (CReturn (m :: CPSFuzz a')) f) = do
  m' <- monadSimplLeft' m
  x <- gfresh "x"
  f' <- monadSimplLeft' (f (CVar x))
  return $ substCPSFuzz x f' m'
monadSimplLeft' term = return term

isCReturn ::
  forall a b m.
  (Typeable a, Typeable b, FreshM m) =>
  (CPSFuzz a -> CPSFuzz (Distr b)) ->
  m (Maybe (a :~~: b))
isCReturn f = do
  x <- gfresh "x"
  case f (CVar x) of
    CReturn (CVar y) ->
      if x == y
      then return $ eqTypeRep (typeRep @a) (typeRep @b)
      else return Nothing
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
monadSimplRight' term = return term

data AnyExpr :: * where
  AnyExpr :: Typeable r => Expr r -> AnyExpr

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
  ( Typeable fs,
    Typeable ts,
    Typeable f,
    Typeable t
  ) =>
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
  (Typeable f, MonadThrowWithStack m) =>
  M.Map String AnyExpr ->
  SIMDFusion f t ->
  m (Expr f)
inject inputs (SIMD1 simd) = do
  case M.lookup (simd ^. fromName) inputs of
    Just (AnyExpr (e :: Expr f')) ->
      case eqTypeRep (typeRep @f) (typeRep @f') of
        Just HRefl -> return e
        _ ->
          throwM_ . InternalError $
            printf
              "inject: expected input %s to have type %s, but it has type %s"
              (simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM_ . InternalError $ printf "inject: unknown input name %s" (simd ^. fromName)
inject inputs (SIMDCons (fusion :: SIMDFusion fs1 _) (simd :: SIMD f1 _)) = do
  acc <- inject inputs fusion
  case M.lookup (simd ^. fromName) inputs of
    Just (AnyExpr (e :: Expr f')) ->
      case eqTypeRep (typeRep @f1) (typeRep @f') of
        Just HRefl -> return $ EPair acc e
        _ ->
          throwM_ . InternalError $
            printf
              "inject: expected input %s to have type %s, but it has type %s"
              (simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM_ . InternalError $ printf "inject: unknown input name %s" (simd ^. fromName)

containsTo :: String -> SIMDFusion f t -> Bool
containsTo x (SIMD1 simd) = (simd ^. toName) == x
containsTo x (SIMDCons fusion simd) =
  containsTo x fusion || (simd ^. toName) == x

project ::
  forall f t r m.
  (Typeable t, Typeable r, MonadThrowWithStack m) =>
  String ->
  SIMDFusion f t ->
  m (Expr (t -> r))
project x (SIMD1 simd) = do
  if simd ^. toName == x
    then case eqTypeRep (typeRep @r) (typeRep @t) of
      Just HRefl -> return . ELam $ \x -> x
      _ ->
        throwM_ . InternalError $
          printf
            "project: expected output name %s to have type (%s), but it has type %s"
            x
            (show $ typeRep @t)
            (show $ typeRep @r)
    else throwM_ . InternalError $ printf "project: unknown output name %s" x
project x (SIMDCons (fusion :: SIMDFusion _ ts1) (simd :: SIMD _ t1)) =
  case (containsTo x fusion, x == simd ^. toName) of
    (True, False) -> do
      projFun <- project x fusion
      return $ (projFun `ecomp` ELam EFst)
    (False, True) ->
      case eqTypeRep (typeRep @t1) (typeRep @r) of
        Just HRefl -> return (ELam ESnd)
        Nothing ->
          throwM_ . InternalError $
            printf
              "project: expected output %s to have type %s, but it has type %s"
              x
              (show $ typeRep @r)
              (show $ typeRep @t1)
    (True, True) ->
      throwM_ . InternalError $
        printf "project: expected output %s to appear on exactly one side" x
    (False, False) ->
      throwM_ . InternalError $
        printf "project: expected output %s to appear on at least one side" x

pullMapEffectsTrans' ::
  forall (row1 :: *) (row2 :: *) m.
  (MonadThrowWithStack m, FreshM m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  String ->
  String ->
  m (Expr (row1 -> row2))
pullMapEffectsTrans' g from to =
  case M.lookup to (g ^. parents) of
    Nothing ->
      throwM_ . InternalError $
        printf "pullMapEffectsTrans': orphaned node %s" to
    Just p ->
      if p == from
        then pullMapEffectsStep' @row1 @row2 g from to
        else do
          case M.lookup p (g ^. types) of
            Nothing ->
              throwM_ . InternalError $
                printf "pullMapEffectsTrans': node %s has no type" p
            Just (SomeTypeRep (parentDb :: TypeRep parentDb)) ->
              -- bunch of yucky proofs to make things kind and type check
              withTypeable parentDb
                $ withKindStar @_ @parentDb
                $ withTypeBag @parentDb
                $ \(_ :: Proxy parentRow) ->
                  withKindStar @_ @parentRow $ do
                    mapParent <- pullMapEffectsTrans' @row1 @parentRow g from p
                    mapTo <- pullMapEffectsTrans' @parentRow @row2 g p to
                    return $ (mapTo `ecomp` mapParent)

pullMapEffectsStep' ::
  forall row1 row2 m.
  (MonadThrowWithStack m, FreshM m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  String ->
  String ->
  m (Expr (row1 -> row2))
pullMapEffectsStep' g from to =
  case M.lookup (from, to) (g ^. edges) of
    Nothing -> throwM_ $ NoSuchEdge from to
    Just (AnyEdge (e :: Edge fromDb toDb)) ->
      case ( eqTypeRep (typeRep @fromDb) (typeRep @(Bag row1)),
             eqTypeRep (typeRep @toDb) (typeRep @(Bag row2))
           ) of
        (Just HRefl, Just HRefl) ->
          case e of
            Map f -> return f
            Sum _ ->
              throwM_ . InternalError $
                printf "pullMapEffectsStep': expected edge (%s, %s) to be a map" from to
        _ ->
          throwM_ . InternalError $
            printf
              "pullMapEffectsStep': expected edge (%s, %s) to have type %s but observed %s"
              from
              to
              (show $ typeRep @(Edge (Bag row1) (Bag row2)))
              (show $ typeRep @(Edge fromDb toDb))

pullClipSumBound' ::
  forall row m.
  (MonadThrowWithStack m, FreshM m, Typeable row, Clip row) =>
  EffectGraph ->
  String ->
  String ->
  m (Vec Number)
pullClipSumBound' g from to =
  case M.lookup (from, to) (g ^. edges) of
    Nothing -> throwM_ $ NoSuchEdge from to
    Just (AnyEdge (e :: Edge bagSum sum)) ->
      withKindStar @_ @bagSum
        $ withKindStar @_ @sum
        $ withTypeBag @bagSum
        $ \(_ :: Proxy sum') ->
          case ( eqTypeRep (typeRep @sum') (typeRep @sum),
                 eqTypeRep (typeRep @sum) (typeRep @row)
               ) of
            (Just HRefl, Just HRefl) -> case e of
              Sum clip ->
                if vecSize @row == length clip
                  then return clip
                  else throwM_ $ ClipSizeError (vecSize @row) (length clip)
              Map _ ->
                throwM_ . InternalError $
                  printf "pullClipSumBound': expected %s to be a sum edge" (show (from, to))
            _ ->
              throwM_ . InternalError $
                printf
                  "pullClipSumBound': expected edge %s to have type %s -> %s, but observed %s -> %s"
                  (show (from, to))
                  (show $ typeRep @(Bag row))
                  (show $ typeRep @row)
                  (show $ typeRep @bagSum)
                  (show $ typeRep @sum)

-- ##################
-- # INFRASTRUCTURE #
-- ##################

withTypeBag ::
  forall db r m.
  (Typeable db, MonadThrowWithStack m) =>
  (forall row. (Typeable row, db ~ Bag row) => Proxy row -> m r) ->
  m r
withTypeBag k =
  case (typeRep @db) of
    App tyCon (tyArg :: TypeRep row) ->
      case eqTypeRep tyCon (typeRep @Bag) of
        Just HRefl -> withTypeable tyArg (k Proxy)
        Nothing ->
          throwM_ . InternalError $
            printf "withTypeBag: expected type %s to be (Bag a)" (show $ typeRep @db)
    _ ->
      throwM_ . InternalError $
        printf "withTypeBag: expected type %s to be (Bag a)" (show $ typeRep @db)

withKindStar :: forall k (a :: k) r m. (Typeable a, MonadThrowWithStack m) => (k ~ * => m r) -> m r
withKindStar k =
  case eqTypeRep (typeRepKind (typeRep @a)) (typeRepKind (typeRep @Int)) of
    Just HRefl -> k
    _ ->
      throwM_ . InternalError $
        printf "withKindStar: expected type %s to have kind *" (show $ typeRep @a)

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError

instance Exception e => Exception (WithCallStack e)

instance Exception e => Show (WithCallStack e) where
  show (WithCallStack err stack) =
    printf "WithCallStack\n  %s\n%s" (show err) (prettyCallStack stack)
