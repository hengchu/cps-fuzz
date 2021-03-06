{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: Compiler
Description: Implementations of all compilation phases.
-}
module Compiler where

import Closure hiding (InternalError)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Constraint
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
import Debug.Trace
import HFunctor
import Names
import Pretty
import Syntax
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf
import Type.Reflection

-- |A single edge in the tree that represents how data flows between bag
-- variables.
data Edge from to where
  Map :: HFix NRedZoneF (from -> to) -> Edge (Bag from) (Bag to)
  Sum :: Vec Number -> Edge (Bag sum) sum

-- |Hides the type indices in 'Edge'.
data AnyEdge where
  AnyEdge :: (Typeable from, Typeable to) => Edge from to -> AnyEdge

-- |Wraps a red-zone computation term, hiding its type index.
data AnyRedZone :: * where
  AnyRedZone :: Typeable r => HFix NRedZoneF r -> AnyRedZone

instance Eq AnyRedZone where
  AnyRedZone (term :: _ r1) == AnyRedZone (term' :: _ r2) =
    case eqTypeRep (typeRep @r1) (typeRep @r2) of
      Just HRefl -> term == term'
      _ -> False

-- |The identifier of a directed edge from some bag variable to another bag
-- variable.
type Direction = (UniqueName, UniqueName)

-- |The tree that connects all bag variables used in the program with bag
-- operations as edges.
data EffectGraph
  = EG
      { _egEdges :: M.Map Direction AnyEdge,
        _egNeighbors :: M.Map UniqueName [UniqueName],
        _egParents :: M.Map UniqueName UniqueName,
        _egTypes :: M.Map UniqueName SomeTypeRep
      }

makeLensesWith abbreviatedFields ''EffectGraph

-- |The empty tree.
emptyEG :: EffectGraph
emptyEG = EG M.empty M.empty M.empty M.empty

-- |The effect tree, and a term that may use data from the tree's leaf nodes.
data Effect r
  = Eff
      { _eGraph :: EffectGraph,
        _eNormalized :: HFix NNormalizedF r
      }

makeLensesWith abbreviatedFields ''Effect

-- |An intermediate data structure used to fuse independent map operations into
-- a single map operation.
data SIMD f t
  = SIMD
      { _sMapFunction :: HFix NRedZoneF (f -> t),
        _sFromName :: UniqueName,
        _sToName :: UniqueName
      }

makeLensesWith abbreviatedFields ''SIMD

-- |Constructs a 'SIMD' value.
simd :: HFix NRedZoneF (f -> t) -> UniqueName -> UniqueName -> SIMD f t
simd = SIMD

-- |Fuse one more map operation through 'SIMD'.
fuse ::
  ( Typeable fs,
    Typeable ts,
    Typeable f,
    Typeable t
  ) =>
  SIMDFusion fs ts ->
  HFix NRedZoneF (f -> t) ->
  UniqueName ->
  UniqueName ->
  SIMDFusion (fs, f) (ts, t)
fuse fusion f from to = SIMDCons fusion (simd f from to)

-- |Extract the 'SIMD' fused map operation.
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

-- |Inject the original inputs from independent map operations into a type suitable
-- for the fused map operation.
injectSimd ::
  forall f t m.
  (Typeable f, MonadThrowWithStack m) =>
  M.Map UniqueName AnyRedZone ->
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
              (show $ simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM' . InternalError $ printf "injectSimd: unknown input name %s" (show $ simd ^. fromName)
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
              (show $ simd ^. fromName)
              (show $ typeRep @f)
              (show $ typeRep @f')
    Nothing -> throwM' . InternalError $ printf "injectSimd: unknown input name %s" (show $ simd ^. fromName)

-- |Checks whether the output of this 'SIMD' fusion contains the given name as
-- an output.
containsTo :: UniqueName -> SIMDFusion f t -> Bool
containsTo x (SIMD1 simd) = (simd ^. toName) == x
containsTo x (SIMDCons fusion simd) =
  containsTo x fusion || (simd ^. toName) == x

-- |Constructs a projection function that picks out the output value from the
-- fused results by the given name.
projectSimd ::
  forall f t r m.
  (Typeable t, Typeable r, MonadThrowWithStack m) =>
  UniqueName ->
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
            (show x)
            (show $ typeRep @t)
            (show $ typeRep @r)
    else throwM' . InternalError $ printf "projectSimd: unknown output name %s" (show x)
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
              (show x)
              (show $ typeRep @r)
              (show $ typeRep @t1)
    (True, True) ->
      throwM' . InternalError $
        printf "projectSimd: expected output %s to appear on exactly one side" (show x)
    (False, False) ->
      throwM' . InternalError $
        printf "projectSimd: expected output %s to appear on at least one side" (show x)

-- |An intermediate data structure that represents a 'SIMD' fused computation.
data SIMDFusion f t where
  SIMD1 :: (Typeable f, Typeable t) => SIMD f t -> SIMDFusion f t
  SIMDCons ::
    (Typeable fs, Typeable ts, Typeable f, Typeable t) =>
    SIMDFusion fs ts ->
    SIMD f t ->
    SIMDFusion (fs, f) (ts, t)

-- |An error message used when the compiler gives up on rewriting to flatten bag
-- operations.
data MisplacedMsg = MisplacedMsg

instance Show MisplacedMsg where
  show _ = "\nI've tried to simplify as much as possible, but there are bag operations that cannot be lifted to the top of the program.\nI do not know how to compile such a program.\nPlease rewrite the following program fragment.\n"

-- |All kinds of errors that the compiler may encounter.
data CompilerError
  -- | An internal error is a bug in the compiler.
  = InternalError String
  | -- | Cannot find the computation step for computing `from` to `to.
    NoSuchEdge {from :: UniqueName, to :: UniqueName}
  | -- | The clip bound has incorrect representation size.
    ClipSizeError {expectedSize :: Int, observedSize :: Int}
    -- | The program uses an illegal red zone term.
  | UnsupportedRedZoneTerm Doc
    -- | The program branches on private information.
  | BranchOnPrivateInformation [UniqueName]
    -- | A loop uses private information as loop condition.
  | LoopUsesPrivateInformation [UniqueName]
    -- | A loop iteration releases private information.
  | LoopIterationReleasesPrivateInformation Doc
    -- | Give up bag operation flattening.
  | MisplacedBagOp MisplacedMsg Doc
  | -- | We need a type that satisfies `Clip`, but
    --  instead got this.
    RequiresClip SomeTypeRep
  | -- | We need a type that satisfies `VecStorable`, but
    --  instead got this.
    RequiresVecStorable SomeTypeRep
    -- | The program's red zone code failed to compile.
  | RequiresRedZone Doc
    -- | The program's orange zone code failed to compile.
  | RequiresOrangeZone Doc
    -- | A term within the "par" marker node is invalid.
  | InvalidParArgument Doc
    -- | A term within the "seq" combinator node is invalid.
  | InvalidSeqArgument Doc
    -- | The "guess" term in above threshold is sensitive.
  | PrivateAboveThresholdGuess Doc
    -- | The "thresh" term in above threshold is sensitive.
  | PrivateAboveThresholdThresh Doc
    -- | Program releases private information.
  | ReleasesPrivateInformation
  deriving (Show)

instance Exception CompilerError

-- |Insert another edge into the effect tree.
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
        Just _ -> throwM' . InternalError $ printf "%s already has a parent" (show to)
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
mergeNeighbors :: M.Map UniqueName [UniqueName] -> M.Map UniqueName [UniqueName] -> M.Map UniqueName [UniqueName]
mergeNeighbors = M.unionWith (++)

-- | Take the disjoint union of the "parents" map.
mergeParents ::
  MonadThrowWithStack m => M.Map UniqueName UniqueName -> M.Map UniqueName UniqueName -> m (M.Map UniqueName UniqueName)
mergeParents m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM' . InternalError $ printf "nodes %s have multiple parents" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

-- | Merge the type annotation map.
mergeTypes ::
  MonadThrowWithStack m =>
  M.Map UniqueName SomeTypeRep ->
  M.Map UniqueName SomeTypeRep ->
  m (M.Map UniqueName SomeTypeRep)
mergeTypes m1 m2 = do
  if M.disjoint m1 m2
    then return (M.union m1 m2)
    else throwM' . InternalError $ printf "nodes %s have multiple types" (show duplicates)
  where
    duplicates = fmap fst (M.toList $ M.intersection m1 m2)

-- | Merge two effect trees.
merge :: MonadThrowWithStack m => EffectGraph -> EffectGraph -> m EffectGraph
merge m1 m2 = do
  m <- mergeEdges (m1 ^. edges) (m2 ^. edges)
  p <- mergeParents (m1 ^. parents) (m2 ^. parents)
  tys <- mergeTypes (m1 ^. types) (m2 ^. types)
  return $ EG m (mergeNeighbors (m1 ^. neighbors) (m2 ^. neighbors)) p tys

-- | Unwrap an 'Effect' value.
unpackEffect :: Effect a -> ((K EffectGraph) :* (HFix NNormalizedF)) a
unpackEffect (Eff a b) = Prod (K a) b

-- | Compile the given term under the context of the specified effect tree,
-- extending the effect tree if required.
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

-- |Part of the steps for extracting effect tree.
traceGraphFlatBagOpF ::
  MonadThrowWithStack m =>
  FlatBagOpF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphFlatBagOpF (FBMapF _ _ kont) = return . K . unK $ kont
traceGraphFlatBagOpF (FBSumF _ _ kont) = return . K . unK $ kont

infixr 6 .<>

-- |Infix combinator for merging two effect trees under the 'K' functor.
(.<>) :: (MonadThrowWithStack m) => K EffectGraph a -> K EffectGraph b -> m (K EffectGraph c)
t1 .<> t2 = do
  t <- merge (unK t1) (unK t2)
  return . K $ t

-- |Part of the steps for extracting effect tree.
traceGraphExprMonadF ::
  MonadThrowWithStack m =>
  ExprMonadF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphExprMonadF (EParF a b) = a .<> b
traceGraphExprMonadF (ELaplaceF _ w) = return . K . unK $ w
traceGraphExprMonadF (EExpF scores) = return . K . unK $ scores
traceGraphExprMonadF (EAboveThresholdF _ secret guess thresh) =
  (secret .<> guess) >>= \g -> g .<> thresh
traceGraphExprMonadF (EBindF m _ k) = m .<> k
traceGraphExprMonadF (EReturnF a) = return . K . unK $ a

-- |Part of the steps for extracting effect tree.
traceGraphExprF ::
  MonadThrowWithStack m =>
  ExprF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphExprF (EVarF _) = return . K $ emptyEG
traceGraphExprF (ELamF _ body) = return . K . unK $ body
traceGraphExprF (EAppF a b) = a .<> b
traceGraphExprF (ECompF bc ab) = bc .<> ab

-- |Part of the steps for extracting effect tree.
traceGraphControlF ::
  MonadThrowWithStack m =>
  ControlF (K EffectGraph) a ->
  m (K EffectGraph a)
traceGraphControlF (CIfF a b c) = (a .<> b) >>= \b' -> b' .<> c
traceGraphControlF (CLoopPureF acc cond iter) = (acc .<> cond) >>= \eff -> eff .<> iter
traceGraphControlF (CLoopF acc cond iter) = (acc .<> cond) >>= \eff -> eff .<> iter

-- |Part of the steps for extracting effect tree.
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
traceGraphPrimF (PAndF a b) = a .<> b
traceGraphPrimF (POrF a b) = a .<> b
traceGraphPrimF (PJustF a) = return . K . unK $ a
traceGraphPrimF PNothingF = return . K $ emptyEG
traceGraphPrimF (PFromJustF a) = return . K . unK $ a
traceGraphPrimF (PIsJustF a) = return . K . unK $ a
traceGraphPrimF (PPairF a b) = a .<> b
traceGraphPrimF (PFstF a) = return . K . unK $ a
traceGraphPrimF (PSndF a) = return . K . unK $ a
traceGraphPrimF (PLengthF a) = return . K . unK $ a
traceGraphPrimF (PIndexF a idx) = a .<> idx
traceGraphPrimF (PSliceF a start end) = (a .<> start) >>= \m -> m .<> end
traceGraphPrimF (PVecLitF as) = go as
  where go []     = return $ K emptyEG
        go (x:xs) = do
          xsG <- go xs
          x .<> xsG
traceGraphPrimF (PConcatF a b) = a .<> b

-- |An f-algebra for extracting effect tree.
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

-- |Extracts the effect tree from a normalized intermediate representation.
effects ::
  MonadThrow m =>
  HFix NNormalizedF a ->
  m (Effect a)
effects =
  hcataM' (prodAlgWithM traceGraphF (return . wrap) combine . hmap unpackEffect)

-- |Part of the steps for opening up bag operations.
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

-- |An f-algebra for opening up bag operations.
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

-- |Open up bag operations.
deflateM ::
  MonadThrowWithStack m =>
  HFix NNormalizedF a ->
  m (HFix MainF a)
deflateM = hcataM' deflateF

-- |A crude approximation of sensitivity values.
data SecurityLevel = Public | Private
  deriving (Show, Eq, Ord)

-- |Join two approximated sensitivity values.
joinSecLvl :: SecurityLevel -> SecurityLevel -> SecurityLevel
joinSecLvl Private _ = Private
joinSecLvl _ Private = Private
joinSecLvl _ _ = Public

-- |How does the program fragment use bag operations?
data UsesBagOp = No | Yes | Bad
  deriving (Show, Eq, Ord)

instance Semigroup UsesBagOp where
  No <> a = a
  Bad <> _ = Bad
  _ <> Bad = Bad
  Yes <> _ = Yes

instance Monoid UsesBagOp where
  mempty = No

-- |Result of "shape checking" a program: checking whether the program properly
-- uses bag operations.
data TermShapeCheck a
  = TSC
      { _tscUsesBagOp :: UsesBagOp,
        _tscPrettified :: P a
      }

makeLensesWith abbreviatedFields ''TermShapeCheck

-- |Part of the steps for shape checking.
termShapeCheck :: MonadThrowWithStack m => HFix NNormalizedF a -> m (TermShapeCheck a)
termShapeCheck =
  hcataM' $
    prodAlgWithM (return . termShapeF) (return . pNNormalizedF) combineTermShapeCheck
      . hmap unpackTermShapeCheck

-- |Combine two separate steps of the shape checking phase.
combineTermShapeCheck :: MonadThrowWithStack m => K UsesBagOp a -> P a -> m (TermShapeCheck a)
combineTermShapeCheck (K Bad) p =
  throwM' $ MisplacedBagOp MisplacedMsg (runPretty p 0)
combineTermShapeCheck a b = return $ TSC (unK a) b

unpackTermShapeCheck :: TermShapeCheck a -> ((K UsesBagOp) :* P) a
unpackTermShapeCheck (TSC a b) = (K a) `Prod` b

-- |An f-algebra for shape checking.
termShapeF :: NNormalizedF (K UsesBagOp) a -> K UsesBagOp a
termShapeF =
  termShapeFlatBagOpF
    `sumAlg` termShapeExprMonadF
    `sumAlg` termShapeExprF
    `sumAlg` termShapeControlF
    `sumAlg` termShapePrimF

-- |Part of the steps for shape checking.
termShapeFlatBagOpF :: FlatBagOpF (K UsesBagOp) a -> K UsesBagOp a
termShapeFlatBagOpF _ = K Yes

-- |Part of the steps for shape checking.
termShapeExprMonadF :: ExprMonadF (K UsesBagOp) a -> K UsesBagOp a
termShapeExprMonadF (EParF (unK -> a) (unK -> b)) =
  case a <> b of
    Yes -> K Bad
    _ -> K $ a <> b
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
termShapeExprMonadF (EExpF (unK -> scores)) =
  case scores of
    Yes -> K Bad
    _ -> K scores
termShapeExprMonadF (EAboveThresholdF _ (unK -> secret) (unK -> guess) (unK -> thresh)) =
  case (secret, guess, thresh) of
    (Yes, _, _) -> K Bad
    (_, Yes, _) -> K Bad
    (_, _, Yes) -> K Bad
    _ -> K $ secret <> guess <> thresh

-- |Part of the steps for shape checking.
termShapeExprF :: ExprF (K UsesBagOp) a -> K UsesBagOp a
termShapeExprF (EVarF _) = K No
termShapeExprF (ELamF _ (unK -> s)) = K s
termShapeExprF (EAppF (unK -> a) (unK -> b)) = K $ a <> b
termShapeExprF (ECompF (unK -> a) (unK -> b)) = K $ a <> b

-- |Part of the steps for shape checking.
termShapeControlF :: ControlF (K UsesBagOp) a -> K UsesBagOp a
termShapeControlF (CIfF (unK -> cond) (unK -> b) (unK -> c)) =
  case cond of
    Yes -> K Bad
    _ -> K $ cond <> b <> c
termShapeControlF (CLoopPureF (unK -> acc) (unK -> cond) (unK -> iter)) =
  case (acc, cond) of
    (Yes, _) -> K Bad
    (_, Yes) -> K Bad
    _ -> K $ acc <> cond <> iter
termShapeControlF (CLoopF (unK -> acc) (unK -> cond) (unK -> iter)) =
  case (acc, cond) of
    (Yes, _) -> K Bad
    (_, Yes) -> K Bad
    _ -> K $ acc <> cond <> iter

-- |Part of the steps for shape checking.
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
termShapePrimF (PAndF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (POrF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PJustF (unK -> a)) = K a
termShapePrimF PNothingF = K No
termShapePrimF (PFromJustF (unK -> a)) = K a
termShapePrimF (PIsJustF (unK -> a)) = K a
termShapePrimF (PPairF (unK -> a) (unK -> b)) = K $ a <> b
termShapePrimF (PFstF (unK -> a)) = K a
termShapePrimF (PSndF (unK -> a)) = K a
termShapePrimF (PLengthF (unK -> a)) = K a
termShapePrimF (PIndexF (unK -> a) (unK -> idx)) = K $ a <> idx
termShapePrimF (PSliceF (unK -> a) (unK -> start) (unK -> end)) = K $ a <> start <> end
termShapePrimF (PVecLitF (foldMap unK -> as)) = K as
termShapePrimF (PConcatF (unK -> a) (unK -> b)) = K $ a <> b

-- |Checks whether an approximated sensitivity value is 'Public'.
isPublic :: SecurityLevel -> Bool
isPublic Public = True
isPublic _ = False

-- |The result of the "deflate bag operation" phase.
data DelayedInflate m a
  = DelayedInflate
      { _diFreeVars :: S.Set UniqueName,
        _diOriginal :: HFix MainF a,
        -- | The inflate algebra builds up a function that takes a list of public
        -- variable names, and emits a term in the MCS language, and a flag of whether
        -- this term evaluates to private or public data.
        _diInflate :: S.Set UniqueName -> m (SecurityLevel, HFix NMcsF a)
      }

-- |Project free variables from 'DelayedInflate'.
prjFvs :: DelayedInflate m a -> K (S.Set UniqueName) a
prjFvs (DelayedInflate fvs _ _) = K fvs

-- |Project the original term from 'DelayedInflate'.
prjOriginal :: DelayedInflate m a -> HFix MainF a
prjOriginal (DelayedInflate _ t _) = t

makeLensesWith abbreviatedFields ''DelayedInflate

-- | Fuse independent map paths through 'SIMD' fusion.
fuseMapPaths ::
  forall (row :: *) r m.
  (Typeable row, MonadThrowWithStack m) =>
  UniqueName ->
  UniqueName ->
  [(UniqueName, SomeTypeRep)] ->
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

-- |Build a term that releases private expressions under the context of an
-- effect tree.
buildReleaseTerm ::
  forall (row :: *) priv pub r m.
  ( Typeable row,
    Typeable priv,
    Typeable pub,
    MonadThrowWithStack m,
    FreshM m
  ) =>
  S.Set UniqueName ->
  EffectGraph ->
  UniqueName ->
  S.Set UniqueName ->
  HFix NOrangeZoneF priv ->
  (HFix NMcsF priv -> HFix NMcsF (Distr pub)) ->
  (forall ts. Typeable ts => Int -> Vec Number -> HFix NMcsF (row -> ts) -> HFix NMcsF (ts -> Distr pub) -> m r) ->
  m r
buildReleaseTerm released g db cFvs cPure build k = do
  let privateSources = S.toList $ cFvs `S.difference` released
  privateSourceTypes <- traverse (getType $ g ^. types) privateSources
  privateSourceParents <- traverse (getParent $ g ^. parents) privateSources
  parentTypes <- traverse (getType $ g ^. types) privateSourceParents
  let parentWithTypes = nub $ zip privateSourceParents parentTypes
  let pvSrcTypeParents = nub $ zip (zip privateSources privateSourceTypes) privateSourceParents
  case privateSources of
    [] ->
      throwM' . InternalError $
        "inflateExprMonadF: impossible, we should have at least 1 private source here"
    _ -> do
      dbrowName <- gfreshAppend db "row"
      let sourceTerms =
            M.fromList
              [ ( dbrowName,
                  AnyRedZone (wrap . hinject' $ EVarF @row (Var dbrowName))
                )
              ]
      fuseMapPaths @row db dbrowName parentWithTypes g $
        \(fusion :: SIMDFusion fs ts) -> do
          mf <- inject' <$> fusedMapFunction fusion
          fusedInput <- injectSimd sourceTerms fusion
          let inj =
                wrap . hinject' @_ @NMcsF $
                  ELamF @row (Var dbrowName) (inject' @_ @NMcsF fusedInput)
          orangeInputName <- gfresh "orange_input"
          let oi = Var @ts orangeInputName
          let oiTerm = wrap . hinject' $ EVarF oi
          releaseTerm <-
            inject' @_ @NMcsF
              <$> foldM (substWithProjection fusion oiTerm) cPure pvSrcTypeParents
          let rls :: HFix NMcsF (ts -> Distr pub)
              rls =
                wrap . hinject' @_ @NMcsF $
                  ELamF oi (build releaseTerm)
          clipBounds <- pullClipSumBounds' g pvSrcTypeParents
          case resolveClip @ts of
            Just dict ->
              withDict dict $
                k (vecSize @ts) clipBounds (mf `compose` inj) rls
            _ -> throwM' $ RequiresClip (SomeTypeRep (typeRep @ts))
  where
    getParent parents x =
      case M.lookup x parents of
        Just p -> return p
        Nothing ->
          throwM' . InternalError $
            printf "inflateExprMonadF: %s has no parent" (show x)
    getType types x =
      case M.lookup x types of
        Just ty -> return ty
        Nothing ->
          throwM' . InternalError $
            printf "inflateExprMonadF: %s has no known type" (show x)
    substWithProjection ::
      forall fs ts a.
      (Typeable ts) =>
      SIMDFusion fs ts ->
      HFix NOrangeZoneF ts ->
      HFix NOrangeZoneF a ->
      ((UniqueName, SomeTypeRep), UniqueName) ->
      m (HFix NOrangeZoneF a)
    substWithProjection fusion releaseInput releaseTerm ((src, srcTy), srcParent) = do
      case srcTy of
        SomeTypeRep (srcTr :: _ srcTy) ->
          withTypeable srcTr
            $ withKindStar @_ @srcTy
            $ do
              projFun <- projectSimd @_ @ts @srcTy srcParent fusion
              return $ hcata' (substGenF (Var @srcTy src) (projFun releaseInput)) releaseTerm

-- |Part of the steps for inflating bag operations.
inflateExprMonadF ::
  forall (row :: *) a m.
  (Typeable row, FreshM m, MonadThrowWithStack m) =>
  EffectGraph ->
  UniqueName ->
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
            buildReleaseTerm @row released g db cFvs cPure (wrap . hinject' . ELaplaceF w) $
              \reprSize clipBounds (mf :: _ (_ -> ts)) rf ->
                case resolveClip @ts of
                  Just dict ->
                    withDict dict
                      $ return
                      $ (Public, wrap . hinject' $ MRunF reprSize clipBounds mf rf)
                  Nothing -> throwM' $ RequiresClip (SomeTypeRep $ typeRep @ts)
inflateExprMonadF g db term@(EExpF scores) =
  let K fvs = fvExprMonadF . hmap prjFvs $ term
      ogTerm = wrap . hinject' $ EExpF (scores ^. original)
      scoresFvs = scores ^. freeVars
  in DelayedInflate fvs ogTerm $ \released -> do
       case scoresFvs `S.isSubsetOf` released of
         True -> do
           (_, scores') <- (scores ^. inflate) released
           return (Public, wrap . hinject' $ EExpF scores')
         False -> do
           scoresPure <- case project' @NOrangeZoneF (scores ^. original) of
             Just scores' -> return scores'
             Nothing -> throwM' . InternalError $
               "inflateExprMonadF: exponential mechanism scores is not a pure term?!"
           buildReleaseTerm @row released g db scoresFvs scoresPure (wrap . hinject' . EExpF) $
             \reprSize clipBounds (mf :: _ (_ -> ts)) rf ->
               case resolveClip @ts of
                 Just dict ->
                   withDict dict $
                   return $ (Public, wrap . hinject' $ MRunF reprSize clipBounds mf rf)
                 Nothing -> throwM' $ RequiresClip (SomeTypeRep $ typeRep @ts)
inflateExprMonadF g db term@(EAboveThresholdF w secret guess thresh) =
  let K fvs = fvExprMonadF . hmap prjFvs $ term
      ogTerm = wrap . hinject' $ EAboveThresholdF w (secret ^. original) (guess ^. original) (thresh ^. original)
      secretFvs = secret ^. freeVars
      guessFvs = guess ^. freeVars
      threshFvs = thresh ^. freeVars
  in DelayedInflate fvs ogTerm $ \released -> do
    when (not $ guessFvs `S.isSubsetOf` released) $
      throwM' $ PrivateAboveThresholdGuess (pMain $ guess ^. original)
    when (not $ threshFvs `S.isSubsetOf` released) $
      throwM' $ PrivateAboveThresholdThresh (pMain $ thresh ^. original)
    (_, guess') <- (guess ^. inflate) released
    (_, thresh') <- (thresh ^. inflate) released
    case secretFvs `S.isSubsetOf` released of
      True -> do
        (_, secret') <- (secret ^. inflate) released
        return (Public, wrap . hinject' $ EAboveThresholdF w secret' guess' thresh')
      False -> do
        secretPure <- case project' @NOrangeZoneF (secret ^. original) of
          Just secret' -> return secret'
          Nothing -> throwM' . InternalError $
            "inflateExprMonadF: above threshold mechanism secret is not a pure term?!"
        buildReleaseTerm @row released g db secretFvs secretPure (\s -> wrap . hinject' $ EAboveThresholdF w s guess' thresh') $
          \reprSize clipBounds (mf :: _ (_ -> ts)) rf ->
            case resolveClip @ts of
              Just dict ->
                withDict dict $
                return $ (Public, wrap . hinject' $ MRunF reprSize clipBounds mf rf)
              Nothing -> throwM' $ RequiresClip (SomeTypeRep $ typeRep @ts)
inflateExprMonadF g db term@(EParF a b) =
  let K fvs = fvExprMonadF . hmap prjFvs $ term
      ogTerm = wrap . hinject' $ EParF (a ^. original) (b ^. original)
      aFvs = a ^. freeVars
      bFvs = b ^. freeVars
   in DelayedInflate fvs ogTerm $ \released -> do
        case ( aFvs `S.isSubsetOf` released,
               bFvs `S.isSubsetOf` released
             ) of
          (True, _) ->
            throwM' $ InvalidParArgument (pMain $ a ^. original)
          (_, True) ->
            throwM' $ InvalidParArgument (pMain $ b ^. original)
          (False, False) -> do
            (a'secLvl, a') <- (a ^. inflate) released
            (b'secLvl, b') <- (b ^. inflate) released
            case ( hproject' @McsF . unwrap $ a',
                   hproject' @McsF . unwrap $ b'
                 ) of
              ( Just (MRunF aReprSize aClipBound (aMf :: _ (arow -> asum)) aRf),
                Just (MRunF bReprSize bClipBound (bMf :: _ (brow -> bsum)) bRf)
                ) ->
                  withHRefl @arow @brow $ \HRefl -> do
                    let reprSize = aReprSize + bReprSize
                    let clipBound = vecConcat aClipBound bClipBound
                    mfInputName <- gfresh "par_map_input"
                    let mfInputVar = Var @arow mfInputName
                    let mfInputTerm = wrap . hinject' $ EVarF mfInputVar
                    let mf = wrap . hinject' $
                          ELamF mfInputVar (pair (aMf %@ mfInputTerm) (bMf %@ mfInputTerm))
                    rfInputName <- gfresh "par_release_input"
                    let rfInputVar = Var @(asum, bsum) rfInputName
                    let rfInputTerm = wrap . hinject' $ EVarF rfInputVar
                    let rf = wrap . hinject' $
                          ELamF rfInputVar $ par (aRf %@ pfst rfInputTerm) (bRf %@ psnd rfInputTerm)
                    let naiveParFusionTerm = wrap . hinject' $ MRunF reprSize clipBound mf rf
                    case eqTypeRep (typeRep @asum) (typeRep @bsum) of
                      Nothing ->
                        return
                          ( a'secLvl `joinSecLvl` b'secLvl,
                            naiveParFusionTerm
                          )
                      Just HRefl -> do
                        case (project' @NRedZoneF aMf,
                              project' @NRedZoneF bMf) of
                          (Just aMf, Just bMf) ->
                            case aMf == bMf of
                              False -> do
                                return
                                  ( a'secLvl `joinSecLvl` b'secLvl,
                                    naiveParFusionTerm
                                  )
                              True -> do
                                -- map functions are identical, we can reduce fused term size
                                let reprSize = aReprSize
                                let clipBound = aClipBound
                                let mf = aMf
                                rfInputName <- gfresh "par_release_input"
                                let rfInputVar = Var @asum rfInputName
                                let rfInputTerm = wrap . hinject' $ EVarF rfInputVar
                                let rf = wrap . hinject' $
                                      ELamF rfInputVar $ par (aRf %@ rfInputTerm) (bRf %@ rfInputTerm)
                                return (a'secLvl `joinSecLvl` b'secLvl,
                                        wrap . hinject' $ MRunF reprSize clipBound (inject' mf) rf)
                          _ -> throwM' . InternalError $
                            printf "inflateExprMonadF: map functions must be red zone code"
              (Nothing, _) ->
                throwM' $ InvalidParArgument (pMain $ a ^. original)
              (_, Nothing) ->
                throwM' $ InvalidParArgument (pMain $ b ^. original)

-- |Part of the steps for inflating bag operations.
inflateGenF ::
  (HInject h MainF, HFunctor h, Monad m) =>
  (h (K (S.Set UniqueName)) a -> K (S.Set UniqueName) a) ->
  h (DelayedInflate m) a ->
  DelayedInflate m a
inflateGenF fvAlgF term =
  let K fvs = fvAlgF . hmap prjFvs $ term
      ogTerm = wrap . hinject' . hmap prjOriginal $ term
   in DelayedInflate fvs ogTerm $ \released -> do
        let secLvl = if fvs `S.isSubsetOf` released then Public else Private
        return (secLvl, inject' ogTerm)

-- |Part of the steps for inflating bag operations.
inflateControlF ::
  MonadThrowWithStack m =>
  ControlF (DelayedInflate m) a ->
  DelayedInflate m a
inflateControlF term@(CIfF cond a b) =
  let K fvs = fvControlF . hmap prjFvs $ term
      ogTerm = wrap . hinject' . hmap prjOriginal $ term
   in DelayedInflate fvs ogTerm $ \released -> do
        let condFvs = cond ^. freeVars
        case condFvs `S.isSubsetOf` released of
          True -> do
            (_, cond') <- (cond ^. inflate) released
            (s1, a') <- (a ^. inflate) released
            (s2, b') <- (b ^. inflate) released
            return (joinSecLvl s1 s2, wrap . hinject' $ CIfF cond' a' b')
          False ->
            throwM' $ BranchOnPrivateInformation $ S.toList (condFvs `S.difference` released)
inflateControlF term@(CLoopPureF acc cond iter) =
  let K fvs = fvControlF . hmap prjFvs $ term
      ogTerm = wrap . hinject' . hmap prjOriginal $ term
   in DelayedInflate fvs ogTerm $ \released -> do
        (accSecLvl, acc') <- (acc ^. inflate) released
        (condSecLvl, cond') <- (cond ^. inflate) released
        (iterSecLvl, iter') <- (iter ^. inflate) released
        return (accSecLvl `joinSecLvl` condSecLvl `joinSecLvl` iterSecLvl,
                wrap . hinject' $ CLoopPureF acc' cond' iter')
inflateControlF term@(CLoopF acc cond iter) =
  let K fvs = fvControlF . hmap prjFvs $ term
      ogTerm = wrap . hinject' . hmap prjOriginal $ term
   in DelayedInflate fvs ogTerm $ \released -> do
        let accFvs = acc ^. freeVars
        let condFvs = cond ^. freeVars
        let iterFvs = iter ^. freeVars
        case ( accFvs `S.isSubsetOf` released,
               condFvs `S.isSubsetOf` released,
               iterFvs `S.isSubsetOf` released
             ) of
          (True, True, True) -> do
            (accSecLvl, acc') <- (acc ^. inflate) released
            (condSecLvl, cond') <- (cond ^. inflate) released
            (iterSecLvl, iter') <- (iter ^. inflate) released
            case iterSecLvl of
              Private ->
                throwM' $
                  LoopIterationReleasesPrivateInformation (pMain ogTerm)
              Public ->
                return
                  ( accSecLvl `joinSecLvl` condSecLvl `joinSecLvl` iterSecLvl,
                    wrap . hinject' $ CLoopF acc' cond' iter'
                  )
          _ ->
            throwM'
              $ LoopUsesPrivateInformation
              $ S.toList
              $ (accFvs `S.union` condFvs `S.union` iterFvs) `S.difference` released

-- |An f-algebra for inflating bag operations.
inflateF ::
  forall (row :: *) a m.
  ( Typeable row,
    MonadThrowWithStack m,
    FreshM m
  ) =>
  EffectGraph ->
  UniqueName ->
  MainF (DelayedInflate m) a ->
  DelayedInflate m a
inflateF g db =
  inflateExprMonadF @row g db
    `sumAlg` inflateGenF fvExprF
    `sumAlg` inflateControlF
    `sumAlg` inflateGenF fvPrimF

-- |Inflate bag operations.
inflateM ::
  forall (row :: *) a m.
  ( Typeable row,
    MonadThrowWithStack m,
    FreshM m
  ) =>
  EffectGraph ->
  UniqueName ->
  HFix MainF a ->
  m (SecurityLevel, HFix NMcsF a)
inflateM g db term = do
  let g' = g & types %~ M.insert db (SomeTypeRep (typeRep @(Bag row)))
  let act = hcata' (inflateF @row g' db) term
  (act ^. inflate) S.empty

-- |Part of the steps of closure conversion.
closureConvertMcsF ::
  ( MonadThrowWithStack m,
    FreshM m
  ) =>
  McsF (HFix NBmcsF) a ->
  m (HFix NBmcsF a)
closureConvertMcsF (MRunF reprSize clip mf rf) =
  case (project' @NRedZoneF mf, project' @MainF rf) of
    (Just mf, Just rf) -> do
      mfClsResult <- closureConvert mf (unK . hcata' (fAnyVarExprF `sumAlg` fAnyVarControlF `sumAlg` fAnyVarPrimF))
      rfClsResult <- closureConvert rf (unK . hcata' (fAnyVarExprMonadF `sumAlg` fAnyVarExprF `sumAlg` fAnyVarControlF `sumAlg` fAnyVarPrimF))
      case (mfClsResult, rfClsResult) of
        (AlreadyClosed mf, AlreadyClosed rf) ->
          return $ wrap . hinject' $ BRunF reprSize clip (wrap . hinject' $ PLitF ()) (inject' mf) (wrap . hinject' $ PLitF ()) (inject' rf)
        (AlreadyClosed mf, Converted (rstate :: _ cls) rf) ->
          case resolveVecStorable @cls of
            Just d ->
              withDict d
                $ return
                $ wrap . hinject'
                $ BRunF reprSize clip (wrap . hinject' $ PLitF ()) (inject' mf) (inject' rstate) (inject' rf)
            _ -> throwM' $ RequiresVecStorable (SomeTypeRep (typeRep @cls))
        (Converted mstate mf, AlreadyClosed rf) ->
          return $ wrap . hinject' $ BRunF reprSize clip (inject' mstate) (inject' mf) (wrap . hinject' $ PLitF ()) (inject' rf)
        (Converted mstate mf, Converted (rstate :: _ cls) rf) ->
          case resolveVecStorable @cls of
            Just d ->
              withDict d
                $ return
                $ wrap . hinject'
                $ BRunF reprSize clip (inject' mstate) (inject' mf) (inject' rstate) (inject' rf)
            _ -> throwM' $ RequiresVecStorable (SomeTypeRep (typeRep @cls))
    (Nothing, _) -> throwM' $ RequiresRedZone (pNBmcs mf)
    (_, Nothing) -> throwM' $ RequiresOrangeZone (pNBmcs rf)

-- |An f-algebra for closure conversion.
closureConvertF ::
  ( MonadThrowWithStack m,
    FreshM m
  ) =>
  NMcsF (HFix NBmcsF) a ->
  m (HFix NBmcsF a)
closureConvertF =
  closureConvertMcsF
    `sumAlgM` (return . wrap . hinject')
    `sumAlgM` (return . wrap . hinject')
    `sumAlgM` (return . wrap . hinject')
    `sumAlgM` (return . wrap . hinject')

-- |Closure conversion.
closureConvertM ::
  ( MonadThrowWithStack m,
    FreshM m
  ) =>
  HFix NMcsF a ->
  m (HFix NBmcsF a)
closureConvertM = hcataM' closureConvertF

-- |Compile a top-level program that takes a bag as input.
compileM ::
  forall row a m.
  (FreshM m, MonadThrowWithStack m, Typeable row, Typeable a) =>
  String ->
  (forall f. HXFix CPSFuzzF f (Bag row) -> HXFix CPSFuzzF f (Distr a)) ->
  m (HFix NBmcsF (Distr a))
compileM db prog = do
  dbName <- gfresh db
  namedTerm <- named' $ prog (xwrap . hinject' $ XEVarF dbName)
  flatTerm <- flatten namedTerm
  let simpleTerm = monadReduce . etaBetaReduce $ flatTerm
  termShapeCheck simpleTerm
  eff <- effects simpleTerm
  deflatedTerm <- deflateM $ eff ^. normalized
  (secLvl, term) <- inflateM @row (eff ^. graph) dbName deflatedTerm
  case secLvl of
    Private -> throwM' ReleasesPrivateInformation
    _ -> return ()
  closureConvertM term

-- |Same as 'compileM', but monomorphises the monad into 'Either SomeException'.
compile ::
  (Typeable row, Typeable a) =>
  String ->
  (forall f. HXFix CPSFuzzF f (Bag row) -> HXFix CPSFuzzF f (Distr a)) ->
  Either SomeException (HFix NBmcsF (Distr a))
compile db prog = flip evalStateT (nameState [UniqueName db 0]) (compileM db prog)

-- |Extract the map path between two bag variables from the effect tree.
pullMapEffectsTrans' ::
  forall (row1 :: *) (row2 :: *) m.
  (MonadThrowWithStack m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  UniqueName ->
  UniqueName ->
  m (HFix NRedZoneF (row1 -> row2))
pullMapEffectsTrans' g from to =
  case M.lookup to (g ^. parents) of
    Nothing ->
      traceShow (from, to)
        $ throwM' . InternalError
        $ printf "pullMapEffectsTrans': orphaned node %s" (show to)
    Just p ->
      if p == from
        then pullMapEffectsStep' @row1 @row2 g from to
        else do
          case M.lookup p (g ^. types) of
            Nothing ->
              throwM' . InternalError $
                printf "pullMapEffectsTrans': node %s has no type" (show p)
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

-- |Extract the map edge between two bag variables from the effect tree.
pullMapEffectsStep' ::
  forall row1 row2 m.
  (MonadThrowWithStack m, Typeable row1, Typeable row2) =>
  EffectGraph ->
  UniqueName ->
  UniqueName ->
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
                printf "pullMapEffectsStep': expected edge (%s, %s) to be a map" (show from) (show to)
        _ ->
          throwM' . InternalError $
            printf
              "pullMapEffectsStep': expected edge (%s, %s) to have type %s but observed %s"
              (show from)
              (show to)
              (show $ typeRep @(Edge (Bag row1) (Bag row2)))
              (show $ typeRep @(Edge fromDb toDb))

-- |Extract the clip bound for a list of bag sum operations from the effect
-- tree.
pullClipSumBounds' ::
  MonadThrowWithStack m =>
  EffectGraph ->
  [((UniqueName, SomeTypeRep), UniqueName)] ->
  m (Vec Number)
pullClipSumBounds' g dirs = do
  bounds <- mapM go dirs
  return $ foldr vecConcat (Vec []) bounds
  where
    go ((t, tr), f) =
      case tr of
        SomeTypeRep (tr :: _ row) ->
          withTypeable tr
            $ withKindStar @_ @row
            $ case resolveClip @row of
              Just dict ->
                withDict dict $ pullClipSumBound' @row g f t
              Nothing -> throwM' $ RequiresClip (SomeTypeRep tr)

-- |Extract the clip bound for a bag sum operation from the effect tree.
pullClipSumBound' ::
  forall row m.
  (MonadThrowWithStack m, Typeable row, Clip row) =>
  EffectGraph ->
  UniqueName ->
  UniqueName ->
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
