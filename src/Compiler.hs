{-# LANGUAGE AllowAmbiguousTypes #-}

module Compiler where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Constraint
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
import HFunctor
import Names
import Syntax
import Text.Printf
import Type.Reflection
import Control.Monad.State.Strict
import Pretty
import Debug.Trace
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Edge from to where
  Map :: HFix NRedZoneF (from -> to) -> Edge (Bag from) (Bag to)
  Sum :: Vec Number -> Edge (Bag sum) sum

data AnyEdge where
  AnyEdge :: (Typeable from, Typeable to) => Edge from to -> AnyEdge

data AnyRedZone :: * where
  AnyRedZone :: Typeable r => HFix NRedZoneF r -> AnyRedZone

type Direction = (String, String)

data EffectGraph
  = EG
      { _egEdges :: M.Map Direction AnyEdge,
        _egNeighbors :: M.Map String [String],
        _egParents :: M.Map String String,
        _egTypes :: M.Map String SomeTypeRep
      }

makeLensesWith abbreviatedFields ''EffectGraph

emptyEG :: EffectGraph
emptyEG = EG M.empty M.empty M.empty M.empty

data Effect r
  = Eff
      { _eGraph :: EffectGraph,
        _eNormalized :: HFix NNormalizedF r
      }

makeLensesWith abbreviatedFields ''Effect

data SIMD f t
  = SIMD
      { _sMapFunction :: HFix NRedZoneF (f -> t),
        _sFromName :: String,
        _sToName :: String
      }

makeLensesWith abbreviatedFields ''SIMD

simd :: HFix NRedZoneF (f -> t) -> String -> String -> SIMD f t
simd = SIMD

fuse ::
  ( Typeable fs,
    Typeable ts,
    Typeable f,
    Typeable t
  ) =>
  SIMDFusion fs ts ->
  HFix NRedZoneF (f -> t) ->
  String ->
  String ->
  SIMDFusion (fs, f) (ts, t)
fuse fusion f from to = SIMDCons fusion (simd f from to)

fusedMapFunction :: FreshM m => SIMDFusion f t -> m (HFix NRedZoneF (f -> t))
fusedMapFunction (SIMD1 simd) = return $ simd ^. mapFunction
fusedMapFunction (SIMDCons (fusion :: _ ai ao) (simd :: _ bi bo)) = do
  f <- fusedMapFunction fusion
  fusedInputName <- gfresh "fused_input"
  let fi = Var fusedInputName
  let fiTerm = wrap . hinject' $ EVarF fi
  let fusedMapFun =
        ELamF fi $
          pair
            (f %@ (pfst fiTerm))
            ((simd ^. mapFunction) %@ (psnd fiTerm))
  return . wrap . hinject' $ fusedMapFun

injectSimd ::
  forall f t m.
  (Typeable f, MonadThrowWithStack m) =>
  M.Map String AnyRedZone ->
  SIMDFusion f t ->
  m (HFix NRedZoneF f)
injectSimd inputs (SIMD1 simd) = do
  case M.lookup (simd ^. fromName) inputs of
    Just (AnyRedZone (e :: _ f')) ->
      case eqTypeRep (typeRep @f) (typeRep @f') of
        Just HRefl -> return e
        _ ->
          throwM' . InternalError $
            printf
              "injectSimd: expected input %s to have type %s, but it has type %s"
              (simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM' . InternalError $ printf "injectSimd: unknown input name %s" (simd ^. fromName)
injectSimd inputs (SIMDCons (fusion :: SIMDFusion fs1 _) (simd :: SIMD f1 _)) = do
  acc <- injectSimd inputs fusion
  case M.lookup (simd ^. fromName) inputs of
    Just (AnyRedZone (e :: _ f')) ->
      case eqTypeRep (typeRep @f1) (typeRep @f') of
        Just HRefl -> return $ pair acc e
        _ ->
          throwM' . InternalError $
            printf
              "injectSimd: expected input %s to have type %s, but it has type %s"
              (simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM' . InternalError $ printf "injectSimd: unknown input name %s" (simd ^. fromName)

containsTo :: String -> SIMDFusion f t -> Bool
containsTo x (SIMD1 simd) = (simd ^. toName) == x
containsTo x (SIMDCons fusion simd) =
  containsTo x fusion || (simd ^. toName) == x

projectSimd ::
  forall f t r m.
  (Typeable t, Typeable r, MonadThrowWithStack m) =>
  String ->
  SIMDFusion f t ->
  m (HFix NRedZoneF t -> HFix NRedZoneF r)
projectSimd x (SIMD1 simd) = do
  if simd ^. toName == x
    then case eqTypeRep (typeRep @r) (typeRep @t) of
      Just HRefl -> do
        return id
      _ ->
        throwM' . InternalError $
          printf
            "projectSimd: expected output name %s to have type (%s), but it has type %s"
            x
            (show $ typeRep @t)
            (show $ typeRep @r)
    else throwM' . InternalError $ printf "projectSimd: unknown output name %s" x
projectSimd x (SIMDCons (fusion :: SIMDFusion _ ts1) (simd :: SIMD _ t1)) =
  case (containsTo x fusion, x == simd ^. toName) of
    (True, False) -> do
      projFun <- projectSimd x fusion
      return $ (projFun . pfst)
    (False, True) ->
      case eqTypeRep (typeRep @t1) (typeRep @r) of
        Just HRefl -> return psnd
        Nothing ->
          throwM' . InternalError $
            printf
              "projectSimd: expected output %s to have type %s, but it has type %s"
              x
              (show $ typeRep @r)
              (show $ typeRep @t1)
    (True, True) ->
      throwM' . InternalError $
        printf "projectSimd: expected output %s to appear on exactly one side" x
    (False, False) ->
      throwM' . InternalError $
        printf "projectSimd: expected output %s to appear on at least one side" x

data SIMDFusion f t where
  SIMD1 :: (Typeable f, Typeable t) => SIMD f t -> SIMDFusion f t
  SIMDCons ::
    (Typeable fs, Typeable ts, Typeable f, Typeable t) =>
    SIMDFusion fs ts ->
    SIMD f t ->
    SIMDFusion (fs, f) (ts, t)

data MisplacedMsg = MisplacedMsg

instance Show MisplacedMsg where
  show _ = "\nI've tried to simplify as much as possible, but there are bag operations that cannot be lifted to the top of the program.\nI do not know how to compile such a program.\nPlease rewrite the following program fragment.\n"

data CompilerError
  = InternalError String
  | -- | Cannot find the computation step for computing `from` to `to.
    NoSuchEdge {from :: String, to :: String}
  | -- | The clip bound has incorrect representation size.
    ClipSizeError {expectedSize :: Int, observedSize :: Int}
  | UnsupportedRedZoneTerm Doc
  | MisplacedBagOp MisplacedMsg Doc
  | -- | We need a type that satisfies `Clip`, but
    --  instead got this.
    RequiresClip SomeTypeRep
  deriving (Show)

instance Exception CompilerError

insert ::
  forall fromType toType m.
  ( MonadThrowWithStack m,
    Typeable fromType,
    Typeable toType
  ) =>
  EffectGraph ->
  Direction ->
  Edge fromType toType ->
  m EffectGraph
insert m dir@(from, to) e =
  case M.lookup dir (m ^. edges) of
    Nothing ->
      case M.lookup to (m ^. parents) of
        Nothing ->
          return $
            m & edges %~ M.insert dir (AnyEdge e)
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
        Just _ -> throwM' . InternalError $ printf "%s already has a parent" to
    Just _ -> throwM' . InternalError $ printf "direction %s is duplicated" (show dir)

-- | Takes the disjoint union of two maps.
mergeEdges ::
  MonadThrowWithStack m =>
  M.Map Direction AnyEdge ->
  M.Map Direction AnyEdge ->
  m (M.Map Direction AnyEdge)
mergeEdges m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM' . InternalError $ printf "directions %s are duplicated" (show duplicates)
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
    else throwM' . InternalError $ printf "nodes %s have multiple parents" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

mergeTypes ::
  MonadThrowWithStack m => M.Map String SomeTypeRep -> M.Map String SomeTypeRep -> m (M.Map String SomeTypeRep)
mergeTypes m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM' . InternalError $ printf "nodes %s have multiple types" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

merge :: MonadThrowWithStack m => EffectGraph -> EffectGraph -> m EffectGraph
merge m1 m2 = do
  m <- mergeEdges (m1 ^. edges) (m2 ^. edges)
  p <- mergeParents (m1 ^. parents) (m2 ^. parents)
  tys <- mergeTypes (m1 ^. types) (m2 ^. types)
  return $ EG m (mergeNeighbors (m1 ^. neighbors) (m2 ^. neighbors)) p tys

unpackEffect :: Effect a -> ((K EffectGraph) :* (HFix NNormalizedF)) a
unpackEffect (Eff a b) = Prod (K a) b

combine :: MonadThrowWithStack m => K EffectGraph a -> HFix NNormalizedF a -> m (Effect a)
combine (unK -> g) term = do
  case hproject' @FlatBagOpF . unwrap $ term of
    Just (FBMapF mf (Var input :: _ (Bag i)) kont) -> do
      (Var output :: _ (Bag o), _) <- openM kont
      case project' @NRedZoneF mf of
        Just mf' -> do
          g' <- insert g (input, output) (Map mf')
          return $ Eff g' term
        _ -> throwM' $ UnsupportedRedZoneTerm (pNNormalized mf)
    Just (FBSumF clip (Var input) kont) -> do
      (Var output, _) <- openM kont
      g' <- insert @(Bag Number) @Number g (input, output) (Sum clip)
      return $ Eff g' term
    _ -> return $ Eff g term

traceGraphFlatBagOpF ::
  MonadThrowWithStack m =>
  FlatBagOpF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphFlatBagOpF (FBMapF _ _ kont) = return . K . unK $ kont
traceGraphFlatBagOpF (FBSumF _ _ kont) = return . K . unK $ kont

infixr 6 .<>

(.<>) :: (MonadThrowWithStack m) => K EffectGraph a -> K EffectGraph b -> m (K EffectGraph c)
t1 .<> t2 = do
  t <- merge (unK t1) (unK t2)
  return . K $ t

traceGraphExprMonadF ::
  MonadThrowWithStack m =>
  ExprMonadF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphExprMonadF (ELaplaceF _ w) = return . K . unK $ w
traceGraphExprMonadF (EBindF m _ k) = m .<> k
traceGraphExprMonadF (EReturnF a) = return . K . unK $ a

traceGraphExprF ::
  MonadThrowWithStack m =>
  ExprF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphExprF (EVarF _) = return . K $ emptyEG
traceGraphExprF (ELamF _ body) = return . K . unK $ body
traceGraphExprF (EAppF a b) = a .<> b
traceGraphExprF (ECompF bc ab) = bc .<> ab

traceGraphControlF ::
  MonadThrowWithStack m =>
  ControlF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphControlF (CIfF a b c) = (a .<> b) >>= \b' -> b' .<> c

traceGraphPrimF ::
  MonadThrowWithStack m =>
  PrimF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphPrimF (PLitF _) = return . K $ emptyEG
traceGraphPrimF (PAddF a b) = a .<> b
traceGraphPrimF (PSubF a b) = a .<> b
traceGraphPrimF (PMultF a b) = a .<> b
traceGraphPrimF (PDivF a b) = a .<> b
traceGraphPrimF (PAbsF a) = return . K . unK $ a
traceGraphPrimF (PSignumF a) = return . K . unK $ a
traceGraphPrimF (PExpF a) = return . K . unK $ a
traceGraphPrimF (PSqrtF a) = return . K . unK $ a
traceGraphPrimF (PLogF a) = return . K . unK $ a
traceGraphPrimF (PGTF a b) = a .<> b
traceGraphPrimF (PGEF a b) = a .<> b
traceGraphPrimF (PLTF a b) = a .<> b
traceGraphPrimF (PLEF a b) = a .<> b
traceGraphPrimF (PEQF a b) = a .<> b
traceGraphPrimF (PNEQF a b) = a .<> b
traceGraphPrimF (PJustF a) = return . K . unK $ a
traceGraphPrimF PNothingF = return . K $ emptyEG
traceGraphPrimF (PFromJustF a) = return . K . unK $ a
traceGraphPrimF (PIsJustF a) = return . K . unK $ a
traceGraphPrimF (PPairF a b) = a .<> b
traceGraphPrimF (PFstF a) = return . K . unK $ a
traceGraphPrimF (PSndF a) = return . K . unK $ a

traceGraphF ::
  MonadThrowWithStack m =>
  NNormalizedF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphF =
  traceGraphFlatBagOpF
    `sumAlgM` traceGraphExprMonadF
    `sumAlgM` traceGraphExprF
    `sumAlgM` traceGraphControlF
    `sumAlgM` traceGraphPrimF

effects ::
  MonadThrow m =>
  HFix NNormalizedF a ->
  m (Effect a)
effects =
  hcataM' (prodAlgWithM traceGraphF (return . wrap) combine . hmap unpackEffect)

deflateFlatBagOpF ::
  MonadThrowWithStack m =>
  FlatBagOpF (HFix MainF) a ->
  m (HFix MainF a)
deflateFlatBagOpF (FBMapF _ _ kont) = do
  (_, kontBody) <- openM kont
  return kontBody
deflateFlatBagOpF (FBSumF _ _ kont) = do
  (_, kontBody) <- openM kont
  return kontBody

deflateF ::
  MonadThrowWithStack m =>
  NNormalizedF (HFix MainF) a ->
  m (HFix MainF a)
deflateF =
  deflateFlatBagOpF
    `sumAlgM` return . wrap . hinject'
    `sumAlgM` return . wrap . hinject'
    `sumAlgM` return . wrap . hinject'
    `sumAlgM` return . wrap . hinject'

deflateM ::
  MonadThrowWithStack m =>
  HFix NNormalizedF a ->
  m (HFix MainF a)
deflateM = hcataM' deflateF

data SecurityLevel = Public | Private
  deriving (Show, Eq, Ord)

data UsesBagOp = No | Yes | Bad
  deriving (Show)

instance Semigroup UsesBagOp where
  No <> a = a
  Bad <> _ = Bad
  _ <> Bad = Bad
  Yes <> _ = Yes

instance Monoid UsesBagOp where
  mempty = No

data TermShapeCheck a = TSC {
  _tscUsesBagOp :: UsesBagOp,
  _tscPrettified :: P a
  }

makeLensesWith abbreviatedFields ''TermShapeCheck

termShapeCheck :: MonadThrowWithStack m => HFix NNormalizedF a -> m (TermShapeCheck a)
termShapeCheck = hcataM' $
  prodAlgWithM (return . termShapeF) (return . pNNormalizedF) combineTermShapeCheck
  . hmap unpackTermShapeCheck

combineTermShapeCheck :: MonadThrowWithStack m => K UsesBagOp a -> P a -> m (TermShapeCheck a)
combineTermShapeCheck (K Bad) p =
  throwM' $ MisplacedBagOp MisplacedMsg (runPretty p 0)
combineTermShapeCheck a b = return $ TSC (unK a) b

unpackTermShapeCheck :: TermShapeCheck a -> ((K UsesBagOp) :* P) a
unpackTermShapeCheck (TSC a b) = (K a) `Prod` b

termShapeF :: NNormalizedF (K UsesBagOp) a -> K UsesBagOp a
termShapeF =
  termShapeFlatBagOpF
  `sumAlg` termShapeExprMonadF
  `sumAlg` termShapeExprF
  `sumAlg` termShapeControlF
  `sumAlg` termShapePrimF

termShapeFlatBagOpF :: FlatBagOpF (K UsesBagOp) a -> K UsesBagOp a
termShapeFlatBagOpF _ = K Yes

termShapeExprMonadF :: ExprMonadF (K UsesBagOp) a -> K UsesBagOp a
termShapeExprMonadF (EReturnF (unK -> s)) =
  case s of
    Yes -> K Bad
    _ -> K s
termShapeExprMonadF (EBindF (unK -> s) _ _) =
  case s of
    Yes -> K Bad
    _ -> K s
termShapeExprMonadF (ELaplaceF _ (unK -> s)) =
  case s of
    Yes -> K Bad
    _ -> K s

termShapeExprF :: ExprF (K UsesBagOp) a -> K UsesBagOp a
termShapeExprF (EVarF _) = K No
termShapeExprF (ELamF _ (unK -> s)) = K s
termShapeExprF (EAppF (unK -> a) (unK -> b)) = K $ a <> b
termShapeExprF (ECompF (unK -> a) (unK -> b)) = K $ a <> b

termShapeControlF :: ControlF (K UsesBagOp) a -> K UsesBagOp a
termShapeControlF (CIfF (unK -> a) (unK -> b) (unK -> c)) = K $ a <> b <> c

termShapePrimF :: PrimF (K UsesBagOp) a -> K UsesBagOp a
termShapePrimF (PLitF _) = K No
termShapePrimF (PAddF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PSubF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PMultF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PDivF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PAbsF (unK -> a)) = K a
termShapePrimF (PSignumF (unK -> a)) = K a
termShapePrimF (PExpF (unK -> a)) = K a
termShapePrimF (PSqrtF (unK -> a)) = K a
termShapePrimF (PLogF (unK -> a)) = K a
termShapePrimF (PGTF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PGEF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PLTF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PLEF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PEQF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PNEQF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PJustF (unK -> a)) = K a
termShapePrimF PNothingF = K No
termShapePrimF (PFromJustF (unK -> a)) = K a
termShapePrimF (PIsJustF (unK -> a)) = K a
termShapePrimF (PPairF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PFstF (unK -> a)) = K a
termShapePrimF (PSndF (unK -> a)) = K a

isPublic :: SecurityLevel -> Bool
isPublic Public = True
isPublic _ = False

data DelayedInflate m a
  = DelayedInflate
      { _diFreeVars :: S.Set String,
        _diOriginal :: HFix MainF a,
        -- | The inflate algebra builds up a function that takes a list of public
        -- variable names, and emits a term in the MCS language, and a flag of whether
        -- this term evaluates to private or public data.
        _diInflate :: S.Set String -> m (SecurityLevel, HFix NMcsF a)
      }

prjFvs :: DelayedInflate m a -> K (S.Set String) a
prjFvs (DelayedInflate fvs _ _) = K fvs

prjOriginal :: DelayedInflate m a -> HFix MainF a
prjOriginal (DelayedInflate _ t _) = t

makeLensesWith abbreviatedFields ''DelayedInflate

fuseMapPaths ::
  forall (row :: *) r m.
  (Typeable row, MonadThrowWithStack m) =>
  String ->
  String ->
  [(String, SomeTypeRep)] ->
  EffectGraph ->
  (forall fs ts. (Typeable fs, Typeable ts) => SIMDFusion fs ts -> m r) ->
  m r
fuseMapPaths _db _dbrowName [] _g _kont =
  throwM' . InternalError $ "fuseMapPaths: expecting at least 1 free variable"
fuseMapPaths db dbrowName [(x, xTR)] g kont =
  case xTR of
    SomeTypeRep (xDbType :: TypeRep xDbType) ->
      withTypeable xDbType
        $ withKindStar @_ @xDbType
        $ withBagRowType @xDbType
        $ \(_ :: Proxy xRowType) ->
          withKindStar @_ @xDbType $ do
            f <- pullMapEffectsTrans' @row @xRowType g db x
            kont (SIMD1 $ simd f dbrowName x)
fuseMapPaths db dbrowName ((x, xTR) : xs) g kont = do
  fuseMapPaths @row db dbrowName [(x, xTR)] g $
    \case
      SIMD1 (SIMD f from to) ->
        fuseMapPaths @row db dbrowName xs g $
          \fusionXs -> kont (fuse fusionXs f from to)
      _ -> throwM' . InternalError $ "fuseMapPaths: expected SIMD1 here"

inflateExprMonadF ::
  forall (row :: *) a m.
  (Typeable row, FreshM m, MonadThrowWithStack m) =>
  EffectGraph ->
  String ->
  ExprMonadF (DelayedInflate m) a ->
  (DelayedInflate m a)
inflateExprMonadF _ _ term@(EBindF m var@(Var bound) f) =
  let K fvs = fvExprMonadF . hmap prjFvs $ term
      ogTerm = wrap . hinject' $ EBindF (m ^. original) var (f ^. original)
   in DelayedInflate fvs ogTerm $ \released -> do
        (m'SecLvl, m') <- (m ^. inflate) released
        (f'SecLvl, f') <- (f ^. inflate) (if isPublic m'SecLvl then S.insert bound released else released)
        return $ (f'SecLvl, wrap . hinject' $ EBindF m' var f')
inflateExprMonadF _ _ term@(EReturnF m) =
  let K fvs = fvExprMonadF . hmap prjFvs $ term
      ogTerm = wrap . hinject' $ EReturnF (m ^. original)
   in DelayedInflate fvs ogTerm $ \released -> do
        (m'SecLvl, m') <- (m ^. inflate) released
        return $ (m'SecLvl, wrap . hinject' $ EReturnF m')
inflateExprMonadF g db term@(ELaplaceF w c) =
  let K fvs = fvExprMonadF . hmap prjFvs $ term
      ogTerm = wrap . hinject' $ ELaplaceF w (c ^. original)
      cFvs = c ^. freeVars
   in DelayedInflate fvs ogTerm $ \released -> do
        case cFvs `S.isSubsetOf` released of
          True -> do
            (_, c') <- (c ^. inflate) released
            return (Public, wrap . hinject' $ ELaplaceF w c')
          False -> do
            cPure <- case project' @NOrangeZoneF (c ^. original) of
              Just c' -> return c'
              Nothing ->
                throwM' . InternalError $ "inflateExprMonadF: laplace center is not a pure term?!"
            let privateSources = S.toList $ cFvs `S.difference` released
            privateSourceTypes <- traverse (getType $ g ^. types) privateSources
            privateSourceParents <- traverse (getParent $ g ^. parents) privateSources
            parentTypes <- traverse (getType $ g ^. types) privateSourceParents
            let parentWithTypes = nub $ zip privateSourceParents parentTypes
            let pvSrcTypeParents = nub $ zip (zip privateSources privateSourceTypes) privateSourceParents
            clipBounds <- pullClipSumBounds' g pvSrcTypeParents
            case privateSources of
              [] ->
                throwM' . InternalError $
                  "inflateExprMonadF: impossible, we should have at least 1 private source here"
              _ -> do
                dbrowName <- gfresh $ printf "%s_row" db
                let sourceTerms = M.fromList [(dbrowName,
                                               AnyRedZone (wrap . hinject' $ EVarF @row (Var dbrowName)))]
                fuseMapPaths @row db dbrowName parentWithTypes g $
                  \(fusion :: SIMDFusion fs ts) -> do
                    mf <- inject' <$> fusedMapFunction fusion
                    fusedInput <- injectSimd sourceTerms fusion
                    let inj = wrap . hinject' @_ @NMcsF $
                              ELamF @row (Var dbrowName) (inject' @_ @NMcsF fusedInput)
                    orangeInputName <- gfresh "orange_input"
                    let oi = Var @ts orangeInputName
                    let oiTerm = wrap . hinject' $ EVarF oi
                    releaseTerm <-
                      inject' @_ @NMcsF
                        <$> foldM (substWithProjection fusion oiTerm) cPure pvSrcTypeParents
                    let rls :: HFix NMcsF (ts -> Distr Number)
                        rls =
                          wrap . hinject' @_ @NMcsF $
                            ELamF oi (wrap . hinject' @_ @NMcsF $ ELaplaceF w releaseTerm)
                    case resolveClip @ts of
                      Nothing -> throwM' $ RequiresClip (SomeTypeRep (typeRep @ts))
                      Just dict ->
                        withDict dict $ do
                          let term =
                                wrap . hinject' @_ @NMcsF $
                                  MRunF (vecSize @ts) clipBounds (mf `compose` inj) rls
                          return (Public, term)
  where
    getParent parents x =
      case M.lookup x parents of
        Just p -> return p
        Nothing ->
          throwM' . InternalError $
            printf "inflateExprMonadF: %s has no parent" x
    getType types x =
      case M.lookup x types of
        Just ty -> return ty
        Nothing ->
          throwM' . InternalError $
            printf "inflateExprMonadF: %s has no known type" x
    substWithProjection ::
      forall fs ts a.
      (Typeable ts) =>
      SIMDFusion fs ts ->
      HFix NOrangeZoneF ts ->
      HFix NOrangeZoneF a ->
      ((String, SomeTypeRep), String) ->
      m (HFix NOrangeZoneF a)
    substWithProjection fusion releaseInput releaseTerm ((src, srcTy), srcParent) = do
      case srcTy of
        SomeTypeRep (srcTr :: _ srcTy) ->
          withTypeable srcTr
            $ withKindStar @_ @srcTy
            $ do
              projFun <- projectSimd @_ @ts @srcTy srcParent fusion
              return $ hcata' (substGenF (Var @srcTy src) (projFun releaseInput)) releaseTerm

inflateGenF ::
  (HInject h MainF, HFunctor h, Monad m) =>
  (h (K (S.Set String)) a -> K (S.Set String) a) ->
  h (DelayedInflate m) a ->
  DelayedInflate m a
inflateGenF fvAlgF term =
  let K fvs = fvAlgF . hmap prjFvs $ term
      ogTerm = wrap . hinject' . hmap prjOriginal $ term
  in DelayedInflate fvs ogTerm $ \released -> do
    let secLvl = if fvs `S.isSubsetOf` released then Public else Private
    return (secLvl, inject' ogTerm)

inflateF :: forall (row :: *) a m.
  (Typeable row,
   MonadThrowWithStack m,
   FreshM m) =>
  EffectGraph ->
  String ->
  MainF (DelayedInflate m) a ->
  DelayedInflate m a
inflateF g db =
  inflateExprMonadF @row g db
  `sumAlg` inflateGenF fvExprF
  `sumAlg` inflateGenF fvControlF
  `sumAlg` inflateGenF fvPrimF

inflateM :: forall (row :: *) a m.
  (Typeable row,
   MonadThrowWithStack m,
   FreshM m) =>
  EffectGraph ->
  String ->
  HFix MainF a ->
  m (SecurityLevel, HFix NMcsF a)
inflateM g db term = do
  let g' = g & types %~ M.insert db (SomeTypeRep (typeRep @(Bag row)))
  let act = hcata' (inflateF @row g' db) term
  (act ^. inflate) S.empty

compileM ::
  forall row a m.
  (FreshM m, MonadThrowWithStack m, Typeable row, Typeable a) =>
  String ->
  (forall f. HXFix CPSFuzzF f (Bag row) -> HXFix CPSFuzzF f (Distr a)) ->
  m (HFix NMcsF (Distr a))
compileM db prog = do
  namedTerm <- named' $ prog (xwrap . hinject' $ XEVarF db)
  flatTerm <- flatten namedTerm
  let simpleTerm = monadReduce . etaBetaReduce $ flatTerm
  termShapeCheck simpleTerm
  eff <- effects simpleTerm
  deflatedTerm <- deflateM $ eff ^. normalized
  (_secLvl, term) <- inflateM @row (eff ^. graph) db deflatedTerm
  -- TODO: log a warning message about secLvl if it's private?
  return term

compile ::
  (Typeable row, Typeable a) =>
  String ->
  (forall f. HXFix CPSFuzzF f (Bag row) -> HXFix CPSFuzzF f (Distr a)) ->
  Either SomeException (HFix NMcsF (Distr a))
compile db prog = flip evalStateT (nameState [db]) (compileM db prog)

pullMapEffectsTrans' ::
  forall (row1 :: *) (row2 :: *) m.
  (MonadThrowWithStack m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  String ->
  String ->
  m (HFix NRedZoneF (row1 -> row2))
pullMapEffectsTrans' g from to =
  case M.lookup to (g ^. parents) of
    Nothing ->
      traceShow (from, to) $
      throwM' . InternalError $
        printf "pullMapEffectsTrans': orphaned node %s" to
    Just p ->
      if p == from
        then pullMapEffectsStep' @row1 @row2 g from to
        else do
          case M.lookup p (g ^. types) of
            Nothing ->
              throwM' . InternalError $
                printf "pullMapEffectsTrans': node %s has no type" p
            Just (SomeTypeRep (parentDb :: TypeRep parentDb)) ->
              -- bunch of yucky proofs to make things kind and type check
              withTypeable parentDb
                $ withKindStar @_ @parentDb
                $ withBagRowType @parentDb
                $ \(_ :: Proxy parentRow) ->
                  withKindStar @_ @parentRow $ do
                    mapParent <- pullMapEffectsTrans' @row1 @parentRow g from p
                    mapTo <- pullMapEffectsTrans' @parentRow @row2 g p to
                    return $ (mapTo `compose` mapParent)

pullMapEffectsStep' ::
  forall row1 row2 m.
  (MonadThrowWithStack m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  String ->
  String ->
  m (HFix NRedZoneF (row1 -> row2))
pullMapEffectsStep' g from to =
  case M.lookup (from, to) (g ^. edges) of
    Nothing -> throwM' $ NoSuchEdge from to
    Just (AnyEdge (e :: Edge fromDb toDb)) ->
      case ( eqTypeRep (typeRep @fromDb) (typeRep @(Bag row1)),
             eqTypeRep (typeRep @toDb) (typeRep @(Bag row2))
           ) of
        (Just HRefl, Just HRefl) ->
          case e of
            Map f -> return f
            Sum _ ->
              throwM' . InternalError $
                printf "pullMapEffectsStep': expected edge (%s, %s) to be a map" from to
        _ ->
          throwM' . InternalError $
            printf
              "pullMapEffectsStep': expected edge (%s, %s) to have type %s but observed %s"
              from
              to
              (show $ typeRep @(Edge (Bag row1) (Bag row2)))
              (show $ typeRep @(Edge fromDb toDb))

pullClipSumBounds' ::
  MonadThrowWithStack m =>
  EffectGraph ->
  [((String, SomeTypeRep), String)] ->
  m (Vec Number)
pullClipSumBounds' g dirs = do
  bounds <- mapM go dirs
  return $ foldr vecConcat (Vec []) bounds
  where go ((t, tr), f) =
          case tr of
            SomeTypeRep (tr :: _ row) ->
              withTypeable tr $
              withKindStar @_ @row $
              case resolveClip @row of
                Just dict ->
                  withDict dict $ pullClipSumBound' @row g f t
                Nothing -> throwM' $ RequiresClip (SomeTypeRep tr)

pullClipSumBound' ::
  forall row m.
  (MonadThrowWithStack m, Typeable row, Clip row) =>
  EffectGraph ->
  String ->
  String ->
  m (Vec Number)
pullClipSumBound' g from to =
  case M.lookup (from, to) (g ^. edges) of
    Nothing -> throwM' $ NoSuchEdge from to
    Just (AnyEdge (e :: Edge bagSum sum)) ->
      withKindStar @_ @bagSum
        $ withKindStar @_ @sum
        $ withBagRowType @bagSum
        $ \(_ :: Proxy sum') ->
          case ( eqTypeRep (typeRep @sum') (typeRep @sum),
                 eqTypeRep (typeRep @sum) (typeRep @row)
               ) of
            (Just HRefl, Just HRefl) -> case e of
              Sum clip ->
                if vecSize @row == length clip
                  then return clip
                  else throwM' $ ClipSizeError (vecSize @row) (length clip)
              Map _ ->
                throwM' . InternalError $
                  printf "pullClipSumBound': expected %s to be a sum edge" (show (from, to))
            _ ->
              throwM' . InternalError $
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

withBagRowType ::
  forall unknown r m.
  (MonadThrowWithStack m, Typeable unknown) =>
  (forall row. (unknown ~ Bag row, Typeable row) => Proxy row -> m r) ->
  m r
withBagRowType k =
  withBagType @unknown @r @m $ \(_ :: Proxy (Bag row)) -> k (Proxy :: _ row)

withKindStar ::
  forall k (a :: k) r m.
  (Typeable a, MonadThrowWithStack m) =>
  (k ~ * => m r) ->
  m r
withKindStar k =
  case eqTypeRep (typeRepKind (typeRep @a)) (typeRepKind (typeRep @Int)) of
    Just HRefl -> k
    _ ->
      throwM' . InternalError $
        printf "withKindStar: expected type %s to have kind *" (show $ typeRep @a)
