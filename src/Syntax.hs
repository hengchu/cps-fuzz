{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: Syntax
Description: Type-indexed AST datatypes of Fuzz, and various intermediate languages used in the Orchard compiler.
-}
module Syntax where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Constraint
import Data.Functor.Compose
import Data.Kind
import Data.Proxy
import qualified Data.Set as S
import Debug.Trace
import GHC.Stack
import GHC.TypeLits
import HFunctor
import qualified Language.Haskell.TH as TH
import Names
import Text.Printf
import Type.Reflection
import Data.Bits
import Data.Hashable

-- |Types that can be serialized as a vector of numbers.
class VecStorable a where
  -- | How many dimensions does it take to store `a`?
  vecSize :: Int

  fromVec :: Vec Number -> a
  asVec :: a -> Vec Number

-- |Types whose vector-format serialization respects addition.
class VecStorable a => VecMonoid a where
  -- | Law:
  -- 1. asVec (a `add` b) = (asVec a) + (asVec b) -- can distribute `asVec`
  -- 2. empty is identity for add
  -- 3. more?
  add :: a -> a -> a

  -- | The identity value for the add operation.
  empty :: a

-- |Types whose vector-format serialization respects clipping.
class VecMonoid a => Clip a where
  -- | Law:
  -- 1. asVec (clip r a) = map (clip r) (asVec a) -- can distribute `clip r`
  -- 2. clip r empty = empty
  -- 3. more?
  clip :: Vec Number -> a -> a

-- | Carries a statically determined name hint for the wrapped value.
newtype Name :: Symbol -> * -> * where
  N :: r -> Name s r

-- | Give a value of type 'r' a statically chosen name 's'.
withName :: r -> Name s r
withName = N

-- | Strip the name.
unName :: Name s r -> r
unName (N r) = r

-- | Types that can be converted to and from deep representations encoded as the fixpoint of the language 'f'.
class Syntactic (f :: * -> *) a where
  -- | The type index of the deep representation.
  type DeepRepr a :: *

  -- | Convert a shallow representation to a deep representation.
  toDeepRepr :: a -> f (DeepRepr a)

  -- | Convert a deep representation to a shallow representation.
  fromDeepRepr :: f (DeepRepr a) -> a

-- | The distribution monad. Note that this is just a stub value. Since we do not actually need to perform any probabilistic computation in the compilation phases, we do not need to use a real distribution monad for the AST representation.
type Distr = Identity

-- | Type of real numbers.
type Number = Double

-- | The shallow representation of monadic values.
newtype Mon f m a = Mon {runMon :: forall b. Typeable b => (a -> f (m b)) -> f (m b)}
  deriving (Functor)

instance Applicative (Mon f m) where
  pure a = Mon $ \k -> k a
  f <*> a = f >>= \f' -> a >>= \a' -> return (f' a')

instance Monad (Mon f m) where
  return a = Mon $ \k -> k a
  Mon m >>= f = Mon $ \k -> m (\a -> runMon (f a) k)

newtype Bag a = Bag [a]
  deriving (Show, Eq, Ord)
  deriving (Functor, Applicative, Monad, Foldable) via []
  deriving Hashable via [a]

newtype Vec a = Vec [a]
  deriving (Show, Eq, Ord)
  deriving (Functor, Applicative, Monad, Foldable) via []
  deriving Hashable via [a]

-- | Type of type-indexed variable names.
data Var (a :: *) where
  Var :: Typeable a => UniqueName -> Var a

instance Hashable (Var a) where
  hashWithSalt salt (Var name) = salt `hashWithSalt` (typeRep @a) `hashWithSalt` name

-- | A wrapper that hides the type-index for variable names.
data AnyVar where
  AnyVar :: Typeable a => Var a -> AnyVar

deriving instance Show (Var a)

deriving instance Eq (Var a)

deriving instance Ord (Var a)

deriving instance Show AnyVar

instance Hashable AnyVar where
  hashWithSalt salt (AnyVar v) = hashWithSalt salt v

instance Eq AnyVar where
  (AnyVar (v :: _ a)) == (AnyVar (u :: _ b)) =
    case eqTypeRep (typeRep @a) (typeRep @b) of
      Just HRefl -> v == u
      _ -> False

instance Ord AnyVar where
  compare (AnyVar (v :: _ a)) (AnyVar (u :: _ b)) =
    let ta = typeRep @a
        tb = typeRep @b
     in case eqTypeRep ta tb of
          Just HRefl -> compare v u
          _ -> compare (SomeTypeRep ta) (SomeTypeRep tb)

-- |A higher-order functor whose fixedpoint corresponds to lambda expressions
-- in a higher-order abstract syntax encoding: variables, lambda abstractions
-- and applications.
data XExprF :: (* -> *) -> * -> * where
  XEVarF :: Typeable a => UniqueName -> XExprF r a
  XELamF :: (KnownSymbol s, Typeable a, Typeable b) => (Name s (r a) -> r b) -> XExprF r (a -> b)
  XEAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> XExprF r b

-- |A higher-order functor whose fixedpoint corresponds to lambda expressions in
-- first-order representation.
data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => Var a -> ExprF r a
  ELamF :: (Typeable a, Typeable b) => Var a -> r b -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b
  ECompF :: (Typeable a, Typeable b, Typeable c) => r (b -> c) -> r (a -> b) -> ExprF r (a -> c)
  deriving (HXFunctor) via (DeriveHXFunctor ExprF)

-- |A higher-order functor whose fixedpoint corresponds to monad expressions
-- in a higher-order abstract syntax encoding.
data XExprMonadF :: (* -> *) -> * -> * where
  XEParF ::
    (Typeable a, Typeable b) =>
    r (Distr a) ->
    r (Distr b) ->
    XExprMonadF r (Distr (a, b))
  XELaplaceF :: Number -> r Number -> XExprMonadF r (Distr Number)
  XEExpF :: r (Vec Number) -> XExprMonadF r (Distr Int)
  XEAboveThresholdF :: Number -> r Number -> r Number -> r Number -> XExprMonadF r (Distr Number)
  XEReturnF :: Typeable a => r a -> XExprMonadF r (Distr a)
  XEBindF ::
    (KnownSymbol s, Typeable a, Typeable b) =>
    r (Distr a) ->
    (Name s (r a) -> r (Distr b)) ->
    XExprMonadF r (Distr b)

-- |A higher-order functor whose fixedpoint corresponds to monad expressions in
-- first-order encoding.
data ExprMonadF :: (* -> *) -> * -> * where
  EParF ::
    (Typeable a, Typeable b) =>
    r (Distr a) ->
    r (Distr b) ->
    ExprMonadF r (Distr (a, b))
  ELaplaceF :: Number -> r Number -> ExprMonadF r (Distr Number)
  EExpF :: r (Vec Number) -> ExprMonadF r (Distr Int)
  EAboveThresholdF :: Number -> r Number -> r Number -> r Number -> ExprMonadF r (Distr Number)
  EReturnF :: Typeable a => r a -> ExprMonadF r (Distr a)
  EBindF :: (Typeable a, Typeable b) => r (Distr a) -> Var a -> r (Distr b) -> ExprMonadF r (Distr b)
  deriving (HXFunctor) via (DeriveHXFunctor ExprMonadF)

-- |Primitives supported by the language.
data PrimF :: (* -> *) -> * -> * where
  -- Arithmetics related stuff.
  PLitF :: (Typeable a, Show a, IsLiteral a) => a -> PrimF r a
  PAddF :: (Num a, Typeable a) => r a -> r a -> PrimF r a
  PSubF :: (Num a, Typeable a) => r a -> r a -> PrimF r a
  PMultF :: (Num a, Typeable a) => r a -> r a -> PrimF r a
  PDivF :: (Fractional a, Typeable a) => r a -> r a -> PrimF r a
  PAbsF :: (Num a, Typeable a) => r a -> PrimF r a
  PSignumF :: (Num a, Typeable a) => r a -> PrimF r a
  PExpF :: (Floating a, Typeable a) => r a -> PrimF r a
  PSqrtF :: (Floating a, Typeable a) => r a -> PrimF r a
  PLogF :: (Floating a, Typeable a) => r a -> PrimF r a
  PGTF :: (Typeable a, Ord a) => r a -> r a -> PrimF r Bool
  PGEF :: (Typeable a, Ord a) => r a -> r a -> PrimF r Bool
  PLTF :: (Typeable a, Ord a) => r a -> r a -> PrimF r Bool
  PLEF :: (Typeable a, Ord a) => r a -> r a -> PrimF r Bool
  PEQF :: (Typeable a, Ord a) => r a -> r a -> PrimF r Bool
  PNEQF :: (Typeable a, Ord a) => r a -> r a -> PrimF r Bool
  PAndF :: r Bool -> r Bool -> PrimF r Bool
  POrF :: r Bool -> r Bool -> PrimF r Bool
  -- Data structures.
  PJustF :: Typeable a => r a -> PrimF r (Maybe a)
  PNothingF :: Typeable a => PrimF r (Maybe a)
  PFromJustF :: Typeable a => r (Maybe a) -> PrimF r a
  PIsJustF :: Typeable a => r (Maybe a) -> PrimF r Bool
  PPairF :: (Typeable a, Typeable b) => r a -> r b -> PrimF r (a, b)
  PFstF :: (Typeable a, Typeable b) => r (a, b) -> PrimF r a
  PSndF :: (Typeable a, Typeable b) => r (a, b) -> PrimF r b
  PLengthF :: Typeable a => r (Vec a) -> PrimF r Int
  PIndexF :: Typeable a => r (Vec a) -> r Int -> PrimF r a
  PSliceF :: Typeable a => r (Vec a) -> r Int -> r Int -> PrimF r (Vec a)
  PVecLitF :: Typeable a => [r a] -> PrimF r (Vec a)
  PConcatF :: Typeable a => r (Vec a) -> r (Vec a) -> PrimF r (Vec a)
  deriving (HXFunctor) via (DeriveHXFunctor PrimF)

-- | Bag operations.
data BagOpF :: (* -> *) -> * -> * where
  BMapF ::
    (Typeable a, Typeable b, Typeable t) =>
    r (a -> b) ->
    r (Bag a) ->
    r (Bag b -> t) ->
    BagOpF r t
  BSumF ::
    Typeable t =>
    Vec Number ->
    r (Bag Number) ->
    r (Number -> t) ->
    BagOpF r t
  deriving (HXFunctor) via (DeriveHXFunctor BagOpF)

-- | Bag operations that had been "flattened", such that the inputs to each bag
-- operation is just a variable name.
data FlatBagOpF :: (* -> *) -> * -> * where
  FBMapF ::
    (Typeable a, Typeable b, Typeable t) =>
    r (a -> b) ->
    Var (Bag a) ->
    r (Bag b -> t) ->
    FlatBagOpF r t
  FBSumF ::
    Typeable t =>
    Vec Number ->
    Var (Bag Number) ->
    r (Number -> t) ->
    FlatBagOpF r t
  deriving (HXFunctor) via (DeriveHXFunctor FlatBagOpF)

-- | Control flow of the language.
data ControlF :: (* -> *) -> * -> * where
  CIfF :: Typeable a => r Bool -> r a -> r a -> ControlF r a
  CLoopPureF :: Typeable a => r a -> r (a -> Bool) -> r (a -> a) -> ControlF r a
  CLoopF :: Typeable a => r a -> r (a -> Bool) -> r (a -> Distr a) -> ControlF r (Distr a)
  deriving (HXFunctor) via (DeriveHXFunctor ControlF)

-- | Almost "BMCS" code. But the "map" and "sum" functions in this format have
-- not gone through closure conversion yet.
data McsF :: (* -> *) -> * -> * where
  MRunF ::
    ( Typeable row,
      Typeable sum,
      Clip sum,
      Typeable result
    ) =>
    Int ->
    Vec Number ->
    r (row -> sum) ->
    r (sum -> Distr result) ->
    McsF r (Distr result)
  deriving (HXFunctor) via (DeriveHXFunctor McsF)

-- | "BMCS" code.
data BmcsF :: (* -> *) -> * -> * where
  BRunF ::
    ( Typeable row,
      Typeable sum,
      Typeable mstate,
      Typeable rstate,
      VecStorable rstate,
      Clip sum,
      Typeable result
    ) =>
    Int ->
    Vec Number ->
    r mstate ->
    r (mstate -> row -> sum) ->
    r rstate ->
    r (rstate -> sum -> Distr result) ->
    BmcsF r (Distr result)

instance HXFunctor XExprF where
  hxmap f g =
    \case
      XEVarF x -> XEVarF x
      XELamF (lam :: Name s _ -> _) -> XELamF @s (f . lam . withName . g . unName)
      XEAppF (f -> lam) (f -> arg) -> XEAppF lam arg

instance HFunctor ExprF where
  hmap f =
    \case
      EVarF x -> EVarF x
      ELamF bound (f -> body) -> ELamF bound body
      EAppF (f -> lam) (f -> body) -> EAppF lam body
      ECompF (f -> bc) (f -> ab) -> ECompF bc ab

instance HFoldable ExprF where
  hfoldMap f =
    \case
      EVarF _ -> mempty
      ELamF _ (f -> body) -> body
      EAppF (f -> lam) (f -> body) -> lam <> body
      ECompF (f -> bc) (f -> ab) -> bc <> ab

instance HTraversable ExprF where
  htraverse f =
    \case
      EVarF x -> pure (EVarF x)
      ELamF bound (f -> mbody) -> ELamF bound <$> mbody
      EAppF (f -> lam) (f -> body) -> EAppF <$> lam <*> body
      ECompF (f -> bc) (f -> ab) -> ECompF <$> bc <*> ab

instance HFunctor ExprMonadF where
  hmap f =
    \case
      EParF (f -> a) (f -> b) -> EParF a b
      ELaplaceF w (f -> c) -> ELaplaceF w c
      EExpF (f -> scores) -> EExpF scores
      EAboveThresholdF w (f -> secret) (f -> guess) (f -> thresh) -> EAboveThresholdF w secret guess thresh
      EReturnF (f -> a) -> EReturnF a
      EBindF (f -> m) bound (f -> k) -> EBindF m bound k

instance HFoldable ExprMonadF where
  hfoldMap f =
    \case
      EParF (f -> a) (f -> b) -> a <> b
      ELaplaceF _ (f -> c) -> c
      EExpF (f -> scores) -> scores
      EAboveThresholdF _ (f -> secret) (f -> guess) (f -> thresh) -> secret <> guess <> thresh
      EReturnF (f -> a) -> a
      EBindF (f -> m) _ (f -> k) -> m <> k

instance HTraversable ExprMonadF where
  htraverse f =
    \case
      EParF (f -> a) (f -> b) -> EParF <$> a <*> b
      ELaplaceF w (f -> c) -> ELaplaceF <$> pure w <*> c
      EExpF (f -> scores) -> EExpF <$> scores
      EAboveThresholdF w (f -> secret) (f -> guess) (f -> thresh) ->
        EAboveThresholdF <$> pure w <*> secret <*> guess <*> thresh
      EReturnF (f -> a) -> EReturnF <$> a
      EBindF (f -> m) bound (f -> k) -> EBindF <$> m <*> pure bound <*> k

instance HXFunctor XExprMonadF where
  hxmap f g =
    \case
      XEParF (f -> a) (f -> b) -> XEParF a b
      XELaplaceF c w -> XELaplaceF c (f w)
      XEExpF (f -> scores) -> XEExpF scores
      XEAboveThresholdF w (f -> secret) (f -> guess) (f -> thresh) ->
        XEAboveThresholdF w secret guess thresh
      XEReturnF a -> XEReturnF (f a)
      XEBindF a (k :: Name s _ -> _) -> XEBindF @s (f a) (f . k . withName . g . unName)

instance HFunctor BagOpF where
  hmap f =
    \case
      BMapF (f -> fun) (f -> db) (f -> kont) -> BMapF fun db kont
      BSumF clip (f -> db) (f -> kont) -> BSumF clip db kont

instance HFoldable BagOpF where
  hfoldMap f =
    \case
      BMapF (f -> fun) (f -> db) (f -> kont) -> fun <> db <> kont
      BSumF _ (f -> db) (f -> kont) -> db <> kont

instance HTraversable BagOpF where
  htraverse f =
    \case
      BMapF (f -> fun) (f -> db) (f -> kont) -> BMapF <$> fun <*> db <*> kont
      BSumF clip (f -> db) (f -> kont) -> BSumF clip <$> db <*> kont

instance HFunctor FlatBagOpF where
  hmap f =
    \case
      FBMapF (f -> fun) db (f -> kont) -> FBMapF fun db kont
      FBSumF clip db (f -> kont) -> FBSumF clip db kont

instance HFoldable FlatBagOpF where
  hfoldMap f =
    \case
      FBMapF (f -> fun) _ (f -> kont) -> fun <> kont
      FBSumF _ _ (f -> kont) -> kont

instance HTraversable FlatBagOpF where
  htraverse f =
    \case
      FBMapF (f -> fun) db (f -> kont) -> FBMapF <$> fun <*> pure db <*> kont
      FBSumF clip db (f -> kont) -> FBSumF clip <$> pure db <*> kont

instance HFunctor ControlF where
  hmap f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> CIfF cond a b
      CLoopPureF (f -> acc) (f -> cond) (f -> iter) -> CLoopPureF acc cond iter
      CLoopF (f -> acc) (f -> cond) (f -> iter) -> CLoopF acc cond iter

instance HFoldable ControlF where
  hfoldMap f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> cond <> a <> b
      CLoopPureF (f -> acc) (f -> cond) (f -> iter) -> acc <> cond <> iter
      CLoopF (f -> acc) (f -> cond) (f -> iter) -> acc <> cond <> iter

instance HTraversable ControlF where
  htraverse f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> CIfF <$> cond <*> a <*> b
      CLoopPureF (f -> acc) (f -> cond) (f -> iter) -> CLoopPureF <$> acc <*> cond <*> iter
      CLoopF (f -> acc) (f -> cond) (f -> iter) -> CLoopF <$> acc <*> cond <*> iter

instance HFunctor PrimF where
  hmap f =
    \case
      PLitF x -> PLitF x
      PAddF (f -> a) (f -> b) -> PAddF a b
      PSubF (f -> a) (f -> b) -> PSubF a b
      PMultF (f -> a) (f -> b) -> PMultF a b
      PDivF (f -> a) (f -> b) -> PDivF a b
      PAbsF (f -> a) -> PAbsF a
      PSignumF (f -> a) -> PSignumF a
      PExpF (f -> a) -> PExpF a
      PSqrtF (f -> a) -> PSqrtF a
      PLogF (f -> a) -> PLogF a
      PGTF (f -> a) (f -> b) -> PGTF a b
      PGEF (f -> a) (f -> b) -> PGEF a b
      PLTF (f -> a) (f -> b) -> PLTF a b
      PLEF (f -> a) (f -> b) -> PLEF a b
      PEQF (f -> a) (f -> b) -> PEQF a b
      PNEQF (f -> a) (f -> b) -> PNEQF a b
      PAndF (f -> a) (f -> b) -> PAndF a b
      POrF (f -> a) (f -> b) -> POrF a b
      PJustF (f -> a) -> PJustF a
      PNothingF -> PNothingF
      PFromJustF (f -> a) -> PFromJustF a
      PIsJustF (f -> a) -> PIsJustF a
      PPairF (f -> a) (f -> b) -> PPairF a b
      PFstF (f -> a) -> PFstF a
      PSndF (f -> a) -> PSndF a
      PLengthF (f -> a) -> PLengthF a
      PIndexF (f -> a) (f -> idx) -> PIndexF a idx
      PSliceF (f -> a) (f -> start) (f -> end) -> PSliceF a start end
      PVecLitF (map f -> as) -> PVecLitF as
      PConcatF (f -> a) (f -> b) -> PConcatF a b

instance HFoldable PrimF where
  hfoldMap f =
    \case
      PLitF _ -> mempty
      PAddF (f -> a) (f -> b) -> a <> b
      PSubF (f -> a) (f -> b) -> a <> b
      PMultF (f -> a) (f -> b) -> a <> b
      PDivF (f -> a) (f -> b) -> a <> b
      PAbsF (f -> a) -> a
      PSignumF (f -> a) -> a
      PExpF (f -> a) -> a
      PSqrtF (f -> a) -> a
      PLogF (f -> a) -> a
      PGTF (f -> a) (f -> b) -> a <> b
      PGEF (f -> a) (f -> b) -> a <> b
      PLTF (f -> a) (f -> b) -> a <> b
      PLEF (f -> a) (f -> b) -> a <> b
      PEQF (f -> a) (f -> b) -> a <> b
      PNEQF (f -> a) (f -> b) -> a <> b
      PAndF (f -> a) (f -> b) -> a <> b
      POrF (f -> a) (f -> b) -> a <> b
      PJustF (f -> a) -> a
      PNothingF -> mempty
      PFromJustF (f -> a) -> a
      PIsJustF (f -> a) -> a
      PPairF (f -> a) (f -> b) -> a <> b
      PFstF (f -> a) -> a
      PSndF (f -> a) -> a
      PLengthF (f -> a) -> a
      PIndexF (f -> a) (f -> idx) -> a <> idx
      PSliceF (f -> a) (f -> start) (f -> end) -> a <> start <> end
      PVecLitF (foldMap f -> as) -> as
      PConcatF (f -> a) (f -> b) -> a <> b

instance HTraversable PrimF where
  htraverse f =
    \case
      PLitF x -> pure $ PLitF x
      PAddF (f -> a) (f -> b) -> PAddF <$> a <*> b
      PSubF (f -> a) (f -> b) -> PSubF <$> a <*> b
      PMultF (f -> a) (f -> b) -> PMultF <$> a <*> b
      PDivF (f -> a) (f -> b) -> PDivF <$> a <*> b
      PAbsF (f -> a) -> PAbsF <$> a
      PSignumF (f -> a) -> PSignumF <$> a
      PExpF (f -> a) -> PExpF <$> a
      PSqrtF (f -> a) -> PSqrtF <$> a
      PLogF (f -> a) -> PLogF <$> a
      PGTF (f -> a) (f -> b) -> PGTF <$> a <*> b
      PGEF (f -> a) (f -> b) -> PGEF <$> a <*> b
      PLTF (f -> a) (f -> b) -> PLTF <$> a <*> b
      PLEF (f -> a) (f -> b) -> PLEF <$> a <*> b
      PEQF (f -> a) (f -> b) -> PEQF <$> a <*> b
      PNEQF (f -> a) (f -> b) -> PNEQF <$> a <*> b
      PAndF (f -> a) (f -> b) -> PAndF <$> a <*> b
      POrF (f -> a) (f -> b) -> POrF <$> a <*> b
      PJustF (f -> a) -> PJustF <$> a
      PNothingF -> pure PNothingF
      PFromJustF (f -> a) -> PFromJustF <$> a
      PIsJustF (f -> a) -> PIsJustF <$> a
      PPairF (f -> a) (f -> b) -> PPairF <$> a <*> b
      PFstF (f -> a) -> PFstF <$> a
      PSndF (f -> a) -> PSndF <$> a
      PLengthF (f -> a) -> PLengthF <$> a
      PIndexF (f -> a) (f -> idx) -> PIndexF <$> a <*> idx
      PSliceF (f -> a) (f -> start) (f -> end) -> PSliceF <$> a <*> start <*> end
      PVecLitF (traverse f -> as) -> PVecLitF <$> as
      PConcatF (f -> a) (f -> b) -> PConcatF <$> a <*> b

instance HFunctor McsF where
  hmap f =
    \case
      MRunF reprSize clip (f -> mf) (f -> rf) ->
        MRunF reprSize clip mf rf

instance HFoldable McsF where
  hfoldMap f =
    \case
      MRunF _ _ (f -> mf) (f -> rf) -> mf <> rf

instance HTraversable McsF where
  htraverse f =
    \case
      MRunF reprSize clip (f -> mf) (f -> rf) ->
        MRunF <$> pure reprSize <*> pure clip <*> mf <*> rf

instance HFunctor BmcsF where
  hmap f =
    \case
      BRunF reprSize clip (f -> mstate) (f -> mf) (f -> rstate) (f -> rf) ->
        BRunF reprSize clip mstate mf rstate rf

instance HFoldable BmcsF where
  hfoldMap f =
    \case
      BRunF _ _ (f -> mstate) (f -> mf) (f -> rstate) (f -> rf) ->
        mstate <> mf <> rstate <> rf

instance HTraversable BmcsF where
  htraverse f =
    \case
      BRunF reprSize clip (f -> mstate) (f -> mf) (f -> rstate) (f -> rf) ->
        BRunF <$> pure reprSize <*> pure clip <*> mstate <*> mf <*> rstate <*> rf

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type CPSFuzzF = BagOpF :+: XExprMonadF :+: XExprF :+: ControlF :+: PrimF

-- | The "core" features of the language: monads, lambda expressions, control
-- flow, and primitive operations.
type MainF = ExprMonadF :+: ExprF :+: ControlF :+: PrimF

-- | CPSFuzz, but with explicit names. This is easier for compilers to handle
-- because everything is first-order.
type NCPSFuzzF = BagOpF :+: MainF

-- | Code that is allowed in the red-zone.
type NRedZoneF = ExprF :+: ControlF :+: PrimF

-- | Code that is allowed in the orange zone.
type NOrangeZoneF = ExprF :+: ControlF :+: PrimF

-- | Morally the same type as NCPSFuzzF, but guaranteed that all input databases
-- are variable names. This lifts all of the bag operations up so that all of
-- the continuations are flattened.
type NNormalizedF = FlatBagOpF :+: MainF

-- | An IR where all bag operations have been compiled to MCS code.
type NMcsF = McsF :+: MainF

-- | The emitted code that can be extracted for BMCS computation.
type NBmcsF = BmcsF :+: MainF

-- | The user-facing embedded language: bag operations, monads, lamba
-- expressions, control flow, and primitives.
type CPSFuzz f = HXFix CPSFuzzF f

-- | Almost the same as 'CPSFuzz', but all HOAS terms are converted to
-- first-order terms so they can be manipulated more easily by the compilation
-- phases.
type NCPSFuzz f = HXFix NCPSFuzzF f

-- ###############################
-- # LANGUAGE SMART CONSTRUCTORS #
-- ###############################

-- |The embedded syntax of comparisons.
class SynOrd (f :: * -> *) where
  infix 4 %<, %<=, %>, %>=, %==, %/=

  (%<) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%<=) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%>) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%>=) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%==) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%/=) :: (Typeable a, Ord a) => f a -> f a -> f Bool

-- |The embedded syntax of monads.
class SynMonad h (m :: * -> *) where
  infixl 1 >>=.
  (>>=.) ::
    (KnownSymbol s, Typeable a, Typeable b) =>
    h (m a) ->
    (Name s (h a) -> h (m b)) ->
    h (m b)
  ret :: Typeable a => h a -> h (m a)

instance SynOrd (CPSFuzz f) where
  a %< b = xwrap . hinject' $ PLTF a b
  a %<= b = xwrap . hinject' $ PLEF a b
  a %> b = xwrap . hinject' $ PGTF a b
  a %>= b = xwrap . hinject' $ PGEF a b
  a %== b = xwrap . hinject' $ PEQF a b
  a %/= b = xwrap . hinject' $ PNEQF a b

instance SynMonad (CPSFuzz f) Distr where
  m >>=. f = xwrap . hinject' $ XEBindF m f
  ret = xwrap . hinject' . XEReturnF

-- |Construct a literal value term.
lit :: (Typeable a, Show a, IsLiteral a) => a -> CPSFuzz f a
lit = xwrap . hinject' . PLitF

-- |Construct a variable term.
var ::
  Typeable a =>
  UniqueName ->
  CPSFuzz f a
var = xwrap . hinject' . XEVarF

-- |Construct a lambda term from a host function.
lam ::
  forall s a b f.
  (KnownSymbol s, Typeable a, Typeable b) =>
  (Name s (CPSFuzz f a) -> CPSFuzz f b) ->
  CPSFuzz f (a -> b)
lam = xwrap . hinject' @XExprF @CPSFuzzF . XELamF

-- |Construct a lambda application term.
app :: (Typeable a, Typeable b) => CPSFuzz f (a -> b) -> CPSFuzz f a -> CPSFuzz f b
app f t = xwrap . hinject' @XExprF @CPSFuzzF $ XEAppF f t

-- |Explicitly share a term, this is basically what let-binding is in a language
-- with an explicit surface syntax.
share :: (Typeable a, Typeable b) => CPSFuzz f a -> CPSFuzz f (a -> b) -> CPSFuzz f b
share = flip app

-- |Laplace mechanism.
lap :: Number -> CPSFuzz f Number -> CPSFuzz f (Distr Number)
lap w c = xwrap . hinject' $ XELaplaceF w c

-- |Above threshold mechanism.
aboveThresh :: Number
            -> CPSFuzz f Number
            -> CPSFuzz f Number
            -> CPSFuzz f Number
            -> CPSFuzz f (Distr Number)
aboveThresh w secret guess thresh =
  xwrap . hinject' $ XEAboveThresholdF w secret guess thresh

-- |Exponential mechanism.
expm :: CPSFuzz f (Vec Number) -> CPSFuzz f (Distr Int)
expm = xwrap . hinject' . XEExpF

-- |The marker 'par' term for combining independent monad terms.
xpar :: (Typeable a, Typeable b) => CPSFuzz f (Distr a) -> CPSFuzz f (Distr b) -> CPSFuzz f (Distr (a, b))
xpar a b = xwrap . hinject' $ XEParF a b

-- |First projection from a tuple.
xpfst :: (Typeable a, Typeable b) => CPSFuzz f (a, b) -> CPSFuzz f a
xpfst a = xwrap . hinject' $ PFstF a

-- |Second projection from a tuple.
xpsnd :: (Typeable a, Typeable b) => CPSFuzz f (a, b) -> CPSFuzz f b
xpsnd a = xwrap . hinject' $ PSndF a

-- |Construct a tuple term.
xppair :: (Typeable a, Typeable b) => CPSFuzz f a -> CPSFuzz f b -> CPSFuzz f (a, b)
xppair a b = xwrap . hinject' $ PPairF a b

infixr 3 %&&
infixr 2 %||

-- |Boolean conjunction.
(%&&) :: CPSFuzz f Bool -> CPSFuzz f Bool -> CPSFuzz f Bool
(%&&) = xpand

-- |Boolean disjunction.
(%||) :: CPSFuzz f Bool -> CPSFuzz f Bool -> CPSFuzz f Bool
(%||) = xpor

-- |Also boolean conjunction.
xpand :: CPSFuzz f Bool -> CPSFuzz f Bool -> CPSFuzz f Bool
xpand a b = xwrap . hinject' $ PAndF a b

-- |Also boolean disjunction.
xpor :: CPSFuzz f Bool -> CPSFuzz f Bool -> CPSFuzz f Bool
xpor a b = xwrap . hinject' $ POrF a b

-- |Construct an conditional branch term.
if_ :: Typeable a => CPSFuzz f Bool -> CPSFuzz f a -> CPSFuzz f a -> CPSFuzz f a
if_ cond t f = xwrap . hinject' $ CIfF cond (toDeepRepr t) (toDeepRepr f)

-- |Construct an explicit loop term.
loop ::
  (Typeable a, KnownSymbol s) =>
  CPSFuzz f a ->
  (Name s (CPSFuzz f a) -> CPSFuzz f Bool) ->
  (Name s (CPSFuzz f a) -> CPSFuzz f (Distr a)) ->
  CPSFuzz f (Distr a)
loop acc cond iter =
  xwrap . hinject' $ CLoopF acc (toDeepRepr cond) (toDeepRepr iter)

-- |Construct an explicit loop term that makes no use of probabilistic effects.
loopPure ::
  (Typeable a, KnownSymbol s) =>
  CPSFuzz f a ->
  (Name s (CPSFuzz f a) -> CPSFuzz f Bool) ->
  (Name s (CPSFuzz f a) -> CPSFuzz f a) ->
  CPSFuzz f a
loopPure acc cond iter =
  xwrap . hinject' $ CLoopPureF acc (toDeepRepr cond) (toDeepRepr iter)

-- |Computes length of a vector.
xlength ::
  Typeable a =>
  CPSFuzz f (Vec a) ->
  CPSFuzz f Int
xlength = xwrap . hinject' . PLengthF

-- |Extracts a slice from a vector.
xslice ::
  Typeable a =>
  CPSFuzz f (Vec a) ->
  CPSFuzz f Int ->
  CPSFuzz f Int ->
  CPSFuzz f (Vec a)
xslice v start end = xwrap . hinject' $ PSliceF v start end

-- |Index into a vector.
xindex ::
  Typeable a =>
  CPSFuzz f (Vec a) ->
  CPSFuzz f Int ->
  CPSFuzz f a
xindex a idx = xwrap . hinject' $ PIndexF a idx

-- |Construct a vector literal.
xvlit ::
  Typeable a =>
  [CPSFuzz f a] ->
  CPSFuzz f (Vec a)
xvlit = xwrap . hinject' . PVecLitF

-- |Concatenate two vectors.
xconcat ::
  Typeable a =>
  CPSFuzz f (Vec a) ->
  CPSFuzz f (Vec a) ->
  CPSFuzz f (Vec a)
xconcat a b = xwrap . hinject' $ PConcatF a b

-- |Construct an optional value.
just :: Typeable a => CPSFuzz f a -> CPSFuzz f (Maybe a)
just = xwrap . hinject' . PJustF

-- |Construct an optional value that is empty.
nothing :: Typeable a => CPSFuzz f (Maybe a)
nothing = xwrap . hinject' $ PNothingF

-- |Tests whether an optional value contains something.
isJust :: Typeable a => CPSFuzz f (Maybe a) -> CPSFuzz f Bool
isJust = xwrap . hinject' . PIsJustF

-- |Unwraps an optional value, assuming it contains some value.
fromJust :: Typeable a => CPSFuzz f (Maybe a) -> CPSFuzz f a
fromJust = xwrap . hinject' . PFromJustF

-- |Construct a bag-map term.
bmap ::
  ( KnownSymbol row,
    KnownSymbol db,
    Typeable a,
    Typeable b,
    Typeable t
  ) =>
  (Name row (CPSFuzz f a) -> CPSFuzz f b) ->
  CPSFuzz f (Bag a) ->
  (Name db (CPSFuzz f (Bag b)) -> CPSFuzz f t) ->
  CPSFuzz f t
bmap f input kont =
  xwrap . hinject' $ BMapF (toDeepRepr f) input (toDeepRepr kont)

-- |Unwraps a bag of optional values, while providing a default value for the
-- optional values that contained nothing.
bmapNothing ::
  (KnownSymbol db, Typeable a, Typeable t) =>
  CPSFuzz f a ->
  CPSFuzz f (Bag (Maybe a)) ->
  (Name db (CPSFuzz f (Bag a)) -> CPSFuzz f t) ->
  CPSFuzz f t
bmapNothing def input kont =
  bmap (\(N row :: Name "maybeRow" _) -> if_ (isJust row) (fromJust row) def) input kont

-- |Filter a bag of values using a function as a predicate.
bfilter ::
  forall row db a t f.
  ( KnownSymbol row,
    KnownSymbol db,
    Typeable a,
    Typeable t
  ) =>
  (Name row (CPSFuzz f a) -> CPSFuzz f Bool) ->
  CPSFuzz f (Bag a) ->
  (Name db (CPSFuzz f (Bag (Maybe a))) -> CPSFuzz f t) ->
  CPSFuzz f t
bfilter pred input kont =
  bmap (\row -> if_ (pred row) (just (unName row)) nothing) input kont

-- |Partition a bag.
bpartition ::
  forall row db a t f.
  ( KnownSymbol row,
    KnownSymbol db,
    Typeable a,
    Typeable t
  ) =>
  -- | The number of partitions we're producing
  Int ->
  -- | The partition function
  (Name row (CPSFuzz f a) -> CPSFuzz f Int) ->
  -- | The input db
  CPSFuzz f (Bag a) ->
  -- | The continuation that processes the parts
  (Name db [CPSFuzz f (Bag (Maybe a))] -> CPSFuzz f t) ->
  CPSFuzz f t
bpartition nparts pfun db kont =
  bpartition' nparts pfun db kont []
  where
    bpartition' n pfun db kont acc
      | n <= 0 = kont (N (reverse acc))
      | otherwise =
        bfilter (\row -> pfun row %== (lit $ n -1)) db $ \(N partn :: Name db _) ->
          bpartition' (n -1) pfun db kont (partn : acc)

-- |Sum a bag of numbers.
bsum ::
  ( KnownSymbol sum,
    Typeable t
  ) =>
  Number ->
  CPSFuzz f (Bag Number) ->
  (Name sum (CPSFuzz f Number) -> CPSFuzz f t) ->
  CPSFuzz f t
bsum clip input kont =
  xwrap . hinject' $ BSumF (Vec [clip]) input (toDeepRepr kont)

instance (Typeable a, Num a, Show a, IsLiteral a) => Num (CPSFuzz f a) where
  a + b = xwrap . hinject' $ PAddF a b
  a * b = xwrap . hinject' $ PMultF a b
  a - b = xwrap . hinject' $ PSubF a b
  abs = xwrap . hinject' . PAbsF
  signum = xwrap . hinject' . PSignumF
  fromInteger = xwrap . hinject' . PLitF . fromInteger

instance (Typeable a, Fractional a, Show a, IsLiteral a) => Fractional (CPSFuzz f a) where
  a / b = xwrap . hinject' $ PDivF a b
  fromRational = xwrap . hinject' . PLitF . fromRational

-- |Constructs an exponentiation term.
exp :: CPSFuzz f Number -> CPSFuzz f Number
exp = xwrap . hinject' . PExpF

-- |Constructs a logarithm term.
log :: CPSFuzz f Number -> CPSFuzz f Number
log = xwrap . hinject' . PLogF

-- |Constructs a square-root term.
sqrt :: CPSFuzz f Number -> CPSFuzz f Number
sqrt = xwrap . hinject' . PSqrtF

infixl 9 %@, `apply`

-- |Infix version of 'xapply'.
(%@) ::
  ( Typeable a,
    Typeable b,
    HInject ExprF h
  ) =>
  HFix h (a -> b) ->
  HFix h a ->
  HFix h b
(%@) = apply

-- |Constructs a HOAS apply term, generically.
xapply ::
    ( Typeable a,
      Typeable b,
      HInject XExprF h
  ) =>
  HXFix h f (a -> b) ->
  HXFix h f a ->
  HXFix h f b
xapply f arg = xwrap . hinject' $ XEAppF f arg

-- |Constructs a first-order apply term, generically.
apply ::
  ( Typeable a,
    Typeable b,
    HInject ExprF h
  ) =>
  HFix h (a -> b) ->
  HFix h a ->
  HFix h b
apply f arg = wrap . hinject' $ EAppF f arg

-- |Constructs a function composition term, generically.
compose ::
  forall h a b c.
  ( HInject ExprF h,
    Typeable a,
    Typeable b,
    Typeable c
  ) =>
  HFix h (b -> c) ->
  HFix h (a -> b) ->
  HFix h (a -> c)
compose bc ab = wrap . hinject' $ ECompF bc ab

-- |Constructs a "par" term.
par ::
  forall h a b.
  ( HInject ExprMonadF h,
    Typeable a,
    Typeable b
  ) =>
  HFix h (Distr a) ->
  HFix h (Distr b) ->
  HFix h (Distr (a, b))
par a b = wrap . hinject' $ EParF a b

-- |Constructs a tuple term.
pair ::
  forall h a b.
  ( HInject PrimF h,
    Typeable a,
    Typeable b
  ) =>
  HFix h a ->
  HFix h b ->
  HFix h (a, b)
pair a b = wrap . hinject' $ PPairF a b

-- |Constructs a first-projection term.
pfst ::
  forall h a b.
  ( HInject PrimF h,
    Typeable a,
    Typeable b
  ) =>
  HFix h (a, b) ->
  HFix h a
pfst = wrap . hinject' . PFstF

-- |Constructs a second-projection term.
psnd ::
  forall h a b.
  ( HInject PrimF h,
    Typeable a,
    Typeable b
  ) =>
  HFix h (a, b) ->
  HFix h b
psnd = wrap . hinject' . PSndF

-- ##################
-- # LANGUAGE TOOLS #
-- ##################

data AnyNCPSFuzz where
  AnyNCPSFuzz :: Typeable a => HFix NCPSFuzzF a -> AnyNCPSFuzz

data UnOpenableTerm = UnOpenableTerm
  deriving (Show)

data UnFlattenableTerm = UnFlattenableTerm
  deriving (Show)

data TypeCheckError
  = TypeCheckError
      { _tceExpected :: SomeTypeRep,
        _tceObserved :: SomeTypeRep
      }
  deriving (Show, Eq, Ord)

data ExpectArrowTypeError
  = ExpectArrowTypeError {_eatObserved :: SomeTypeRep}
  deriving (Show, Eq, Ord)

data ExpectTyConTypeError con
  = ExpectTyConTypeError
      { _etctExpected :: TypeRep con,
        _etctObserved :: SomeTypeRep
      }
  deriving (Show, Eq, Ord)

type ExpectBagTypeError = ExpectTyConTypeError Bag

type ExpectMaybeTypeError = ExpectTyConTypeError Maybe

instance Exception UnOpenableTerm

instance Exception UnFlattenableTerm

instance Exception TypeCheckError

instance Exception ExpectArrowTypeError

instance (Typeable k, Typeable a) => Exception (ExpectTyConTypeError (a :: k))

type MonadThrowWithStack m = (MonadThrow m, HasCallStack)

data WithCallStack exc = WithCallStack CallStack exc

instance Show exc => Show (WithCallStack exc) where
  show (WithCallStack cs exc) =
    printf "error:\n%s\ncallstack:\n%s" (show exc) (prettyCallStack cs)

instance Exception exc => Exception (WithCallStack exc)

throwM' :: (MonadThrowWithStack m, Exception e) => e -> m a
throwM' = throwM . WithCallStack callStack

withArrowType ::
  forall unknown r m.
  (MonadThrowWithStack m, Typeable unknown) =>
  (forall a b. (unknown ~ (a -> b), Typeable a, Typeable b) => Proxy (a -> b) -> m r) ->
  m r
withArrowType k =
  case trUnknown of
    App someArrow (b :: TypeRep b) -> case someArrow of
      App arrow (a :: TypeRep a) -> case eqTypeRep arrow (typeRep @(->)) of
        Just HRefl -> case eqTypeRep trUnknown (typeRep @(a -> b)) of
          Just HRefl -> withTypeable a $ withTypeable b $ k Proxy
          _ -> throwM' $ TypeCheckError (SomeTypeRep $ typeRep @(a -> b)) (SomeTypeRep trUnknown)
        _ -> throwM' . ExpectArrowTypeError $ (SomeTypeRep trUnknown)
      _ -> throwM' . ExpectArrowTypeError $ (SomeTypeRep trUnknown)
    _ -> throwM' . ExpectArrowTypeError $ (SomeTypeRep trUnknown)
  where
    trUnknown = typeRep @unknown

withBagType ::
  forall unknown r m.
  (MonadThrowWithStack m, Typeable unknown) =>
  (forall a. (unknown ~ Bag a, Typeable a) => Proxy (Bag a) -> m r) ->
  m r
withBagType k =
  case trUnknown of
    App someTyCon (a :: TypeRep a) ->
      case eqTypeRep someTyCon (typeRep @Bag) of
        Just HRefl -> case eqTypeRep trUnknown (typeRep @(Bag a)) of
          Just HRefl -> withTypeable a (k Proxy)
          _ -> throwM' $ ExpectTyConTypeError (typeRep @Bag) (SomeTypeRep trUnknown)
        _ -> throwM' $ ExpectTyConTypeError (typeRep @Bag) (SomeTypeRep trUnknown)
    _ -> throwM' $ ExpectTyConTypeError (typeRep @Bag) (SomeTypeRep trUnknown)
  where
    trUnknown = typeRep @unknown

withHRefl ::
  forall a b m r.
  (Typeable a, Typeable b, MonadThrowWithStack m) =>
  ((a :~~: b) -> m r) ->
  m r
withHRefl k =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> k HRefl
    Nothing -> throwM' $ TypeCheckError (SomeTypeRep (typeRep @a)) (SomeTypeRep (typeRep @b))

fvBagOpF ::
  BagOpF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvBagOpF (BMapF (unK -> f) (unK -> db) (unK -> kont)) = K $ f <> db <> kont
fvBagOpF (BSumF _ (unK -> db) (unK -> kont)) = K $ db <> kont

fvBagOpFM ::
  FreshM m =>
  BagOpF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvBagOpFM (BMapF (unK -> f) (unK -> db) (unK -> kont)) = K $ S.union <$> f <*> (S.union <$> db <*> kont)
fvBagOpFM (BSumF _ (unK -> db) (unK -> kont)) = K $ S.union <$> db <*> kont

namedBagOpFM ::
  (MonadThrowWithStack m, FreshM m) =>
  BagOpF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedBagOpFM (BMapF ((unK -> f) :: _ (a -> b)) ((unK -> db) :: _ (Bag a)) ((unK -> kont) :: _ (Bag b -> t))) = K $ do
  f' <- f
  db' <- db
  kont' <- kont
  case (f', db', kont') of
    ( AnyNCPSFuzz (f' :: _ (a_arrow_b)),
      AnyNCPSFuzz (db' :: _ baga),
      AnyNCPSFuzz (kont' :: _ (bagb_arrow_t))
      ) ->
        withHRefl @(a -> b) @a_arrow_b $ \HRefl ->
          withHRefl @(Bag a) @baga $ \HRefl ->
            withHRefl @(Bag b -> t) @bagb_arrow_t $ \HRefl ->
              return . AnyNCPSFuzz . wrap . hinject' $ BMapF f' db' kont'
namedBagOpFM (BSumF clip ((unK -> db) :: _ (Bag Number)) ((unK -> kont) :: _ (Number -> t))) = K $ do
  db' <- db
  kont' <- kont
  case (db', kont') of
    ( AnyNCPSFuzz (db' :: _ bagnum),
      AnyNCPSFuzz (kont' :: _ num_arrow_t)
      ) ->
        withHRefl @(Bag Number) @bagnum $ \HRefl ->
          withHRefl @(Number -> t) @num_arrow_t $ \HRefl ->
            return . AnyNCPSFuzz . wrap . hinject' $ BSumF clip db' kont'

fAnyVarExprMonadF ::
  ExprMonadF (K (S.Set AnyVar)) a ->
  K (S.Set AnyVar) a
fAnyVarExprMonadF (EParF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarExprMonadF (ELaplaceF _ (unK -> fvs)) = K fvs
fAnyVarExprMonadF (EExpF (unK -> scores)) = K scores
fAnyVarExprMonadF (EAboveThresholdF _ (unK -> secret) (unK -> guess) (unK -> thresh)) =
  K $ secret <> guess <> thresh
fAnyVarExprMonadF (EReturnF (unK -> fvs)) = K fvs
fAnyVarExprMonadF (EBindF (unK -> fvs1) bound (unK -> fvs2)) =
  K $
    fvs1 `S.union` S.delete (AnyVar bound) fvs2

fvExprMonadF ::
  ExprMonadF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvExprMonadF (EParF (unK -> a) (unK -> b)) = K $ a <> b
fvExprMonadF (ELaplaceF _ (unK -> fvs)) = K fvs
fvExprMonadF (EExpF (unK -> scores)) = K scores
fvExprMonadF (EAboveThresholdF _ (unK -> secret) (unK -> guess) (unK -> thresh)) =
  K $ secret <> guess <> thresh
fvExprMonadF (EReturnF (unK -> fvs)) = K fvs
fvExprMonadF (EBindF (unK -> fvs1) (Var bound) (unK -> fvs2)) =
  K $
    fvs1 `S.union` S.delete bound fvs2

fvXExprMonadF ::
  FreshM m =>
  XExprMonadF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvXExprMonadF (XEParF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvXExprMonadF (XELaplaceF _ (unK -> w)) = K w
fvXExprMonadF (XEExpF (unK -> scores)) = K scores
fvXExprMonadF (XEAboveThresholdF _ (unK -> secret) (unK -> guess) (unK -> thresh)) =
  K $ S.union <$> secret <*> (S.union <$> guess <*> thresh)
fvXExprMonadF (XEReturnF (unK -> a)) = K a
fvXExprMonadF (XEBindF (unK -> m) f) = K $ do
  m' <- m
  x <- gfresh "x"
  f' <- unK $ f (N . K . return . S.singleton $ x)
  return $ S.union m' (S.delete x f')

namedXExprMonadFM ::
  (MonadThrowWithStack m, FreshM m) =>
  XExprMonadF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedXExprMonadFM (XEParF ((unK -> a) :: _ (Distr a)) ((unK -> b) :: _ (Distr b))) = K $ do
  a' <- a
  b' <- b
  case (a', b') of
    ( AnyNCPSFuzz (a' :: _ distra),
      AnyNCPSFuzz (b' :: _ distrb)
      ) ->
        withHRefl @(Distr a) @distra $ \HRefl ->
          withHRefl @(Distr b) @distrb $ \HRefl -> do
            return $ AnyNCPSFuzz . wrap . hinject' $ EParF a' b'
namedXExprMonadFM (XEReturnF (unK -> a)) = K $ do
  AnyNCPSFuzz a' <- a
  return . AnyNCPSFuzz . wrap . hinject' $ EReturnF a'
namedXExprMonadFM (XEBindF ((unK -> m) :: _ (Distr a)) (lam :: Name s _ -> _ (Distr b))) = K $ do
  m' <- m
  x <- gfresh (symbolVal (Proxy :: _ s))
  let var = Var @a x
  f' <- unK $ lam (N . K . return . AnyNCPSFuzz . wrap . hinject' $ EVarF var)
  case (m', f') of
    ( AnyNCPSFuzz (m' :: _ distr_a),
      AnyNCPSFuzz (f' :: _ distr_b)
      ) ->
        withHRefl @(Distr a) @distr_a $ \HRefl ->
          withHRefl @(Distr b) @distr_b $ \HRefl ->
            return . AnyNCPSFuzz . wrap . hinject' $ EBindF m' var f'
namedXExprMonadFM (XELaplaceF w ((unK -> c) :: _ Number)) = K $ do
  c' <- c
  case c' of
    AnyNCPSFuzz (c' :: _ num) ->
      withHRefl @Number @num $ \HRefl ->
        return . AnyNCPSFuzz . wrap . hinject' $ ELaplaceF w c'
namedXExprMonadFM (XEExpF ((unK -> scores) :: _ vecnumber)) = K $ do
  scores' <- scores
  case scores' of
    AnyNCPSFuzz (scores' :: _ vecnumber') ->
      withHRefl @vecnumber @vecnumber' $ \HRefl ->
      return . AnyNCPSFuzz . wrap . hinject' $ EExpF scores'
namedXExprMonadFM (XEAboveThresholdF w (unK -> secret) (unK -> guess) (unK -> thresh)) = K $ do
  secret' <- secret
  guess' <- guess
  thresh' <- thresh
  case (secret', guess', thresh') of
    (AnyNCPSFuzz (secret' :: _ num1),
     AnyNCPSFuzz (guess' :: _ num2),
     AnyNCPSFuzz (thresh' :: _ num3)) ->
      withHRefl @Number @num1 $ \HRefl ->
      withHRefl @Number @num2 $ \HRefl ->
      withHRefl @Number @num3 $ \HRefl ->
      return . AnyNCPSFuzz . wrap . hinject' $ EAboveThresholdF w secret' guess' thresh'

fvXExprF ::
  FreshM m =>
  XExprF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvXExprF (XEVarF x) = K . return . S.singleton $ x
fvXExprF (XELamF f) = K $ do
  x <- gfresh "x"
  f' <- unK $ f (N . K . return . S.singleton $ x)
  return $ S.delete x f'
fvXExprF (XEAppF (unK -> a) (unK -> b)) =
  K $ S.union <$> a <*> b

namedXExprFM ::
  forall a m.
  (MonadThrowWithStack m, FreshM m) =>
  XExprF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedXExprFM (XEVarF x) = K . return . AnyNCPSFuzz . wrap . hinject' $ EVarF (Var @a x)
namedXExprFM (XELamF (f :: Name s (_ a1) -> _ b)) = K $ do
  x <- gfresh (symbolVal (Proxy :: _ s))
  let var = Var @a1 x
  f' <- unK $ f (N . K . return . AnyNCPSFuzz . wrap . hinject' $ EVarF var)
  case f' of
    AnyNCPSFuzz (f' :: _ b') ->
      withHRefl @b @b' $ \HRefl ->
        return . AnyNCPSFuzz . wrap . hinject' $ ELamF var f'
namedXExprFM (XEAppF ((unK -> a) :: _ (a1 -> a)) ((unK -> b) :: _ a1)) = K $ do
  a' <- a
  b' <- b
  case (a', b') of
    ( AnyNCPSFuzz (a' :: _ a1_arrow_a),
      AnyNCPSFuzz (b' :: _ a1')
      ) ->
        withHRefl @(a1 -> a) @a1_arrow_a $ \HRefl ->
          withHRefl @a1 @a1' $ \HRefl ->
            return . AnyNCPSFuzz . wrap . hinject' $ EAppF a' b'

fAnyVarControlF ::
  ControlF (K (S.Set AnyVar)) a ->
  K (S.Set AnyVar) a
fAnyVarControlF (CIfF (unK -> cond) (unK -> a) (unK -> b)) = K $ cond <> a <> b
fAnyVarControlF (CLoopPureF (unK -> acc) (unK -> cond) (unK -> iter)) = K $ acc <> cond <> iter
fAnyVarControlF (CLoopF (unK -> acc) (unK -> cond) (unK -> iter)) = K $ acc <> cond <> iter

fvControlF ::
  ControlF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvControlF (CIfF (unK -> cond) (unK -> a) (unK -> b)) = K $ cond <> a <> b
fvControlF (CLoopPureF (unK -> acc) (unK -> cond) (unK -> iter)) = K $ acc <> cond <> iter
fvControlF (CLoopF (unK -> acc) (unK -> cond) (unK -> iter)) = K $ acc <> cond <> iter

fvControlFM ::
  FreshM m =>
  ControlF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvControlFM (CIfF (unK -> cond) (unK -> a) (unK -> b)) =
  K $ S.union <$> cond <*> (S.union <$> a <*> b)
fvControlFM (CLoopPureF (unK -> acc) (unK -> cond) (unK -> iter)) =
  K $ S.union <$> acc <*> (S.union <$> cond <*> iter)
fvControlFM (CLoopF (unK -> acc) (unK -> cond) (unK -> iter)) =
  K $ S.union <$> acc <*> (S.union <$> cond <*> iter)

namedControlFM ::
  forall a m.
  (MonadThrowWithStack m, FreshM m) =>
  ControlF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedControlFM (CIfF (unK -> cond) (unK -> a) (unK -> b)) = K $ do
  cond' <- cond
  a' <- a
  b' <- b
  case (cond', a', b') of
    ( AnyNCPSFuzz (cond' :: _ bool),
      AnyNCPSFuzz (a' :: _ a'),
      AnyNCPSFuzz (b' :: _ b')
      ) ->
        withHRefl @Bool @bool $ \HRefl ->
          withHRefl @a @a' $ \HRefl ->
            withHRefl @a @b' $ \HRefl ->
              return . AnyNCPSFuzz . wrap . hinject' $ CIfF cond' a' b'
namedControlFM (CLoopPureF ((unK -> acc) :: _ acc) ((unK -> cond) :: _ (acc -> Bool)) ((unK -> iter) :: _ (acc -> acc))) = K $ do
  acc' <- acc
  cond' <- cond
  iter' <- iter
  case (acc', cond', iter') of
    ( AnyNCPSFuzz (acc' :: _ acc'),
      AnyNCPSFuzz (cond' :: _ acc_arrow_bool'),
      AnyNCPSFuzz (iter' :: _ acc_arrow_acc')
      ) ->
        withHRefl @acc @acc' $ \HRefl ->
          withHRefl @(acc -> Bool) @acc_arrow_bool' $ \HRefl ->
            withHRefl @(acc -> acc) @(acc_arrow_acc') $ \HRefl ->
              return . AnyNCPSFuzz . wrap . hinject' $ CLoopPureF acc' cond' iter'
namedControlFM (CLoopF ((unK -> acc) :: _ acc) ((unK -> cond) :: _ (acc -> Bool)) ((unK -> iter) :: _ (acc -> Distr acc))) = K $ do
  acc' <- acc
  cond' <- cond
  iter' <- iter
  case (acc', cond', iter') of
    ( AnyNCPSFuzz (acc' :: _ acc'),
      AnyNCPSFuzz (cond' :: _ acc_arrow_bool'),
      AnyNCPSFuzz (iter' :: _ acc_arrow_distr_acc')
      ) ->
        withHRefl @acc @acc' $ \HRefl ->
          withHRefl @(acc -> Bool) @acc_arrow_bool' $ \HRefl ->
            withHRefl @(acc -> Distr acc) @(acc_arrow_distr_acc') $ \HRefl ->
              return . AnyNCPSFuzz . wrap . hinject' $ CLoopF acc' cond' iter'

fvPrimF ::
  PrimF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvPrimF (PLitF _) = K mempty
fvPrimF (PAddF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PSubF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PMultF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PDivF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PAbsF (unK -> a)) = K a
fvPrimF (PSignumF (unK -> a)) = K a
fvPrimF (PExpF (unK -> a)) = K a
fvPrimF (PSqrtF (unK -> a)) = K a
fvPrimF (PLogF (unK -> a)) = K a
fvPrimF (PGTF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PGEF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PLTF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PLEF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PEQF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PNEQF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PAndF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (POrF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PJustF (unK -> a)) = K a
fvPrimF PNothingF = K mempty
fvPrimF (PFromJustF (unK -> a)) = K a
fvPrimF (PIsJustF (unK -> a)) = K a
fvPrimF (PPairF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PFstF (unK -> a)) = K a
fvPrimF (PSndF (unK -> a)) = K a
fvPrimF (PLengthF (unK -> a)) = K a
fvPrimF (PIndexF (unK -> a) (unK -> idx)) = K $ a <> idx
fvPrimF (PSliceF (unK -> a) (unK -> start) (unK -> end)) = K $ a <> start <> end
fvPrimF (PVecLitF (foldMap id . map unK -> as)) = K $ as
fvPrimF (PConcatF (unK -> a) (unK -> b)) = K $ a <> b

fAnyVarPrimF ::
  PrimF (K (S.Set AnyVar)) a ->
  K (S.Set AnyVar) a
fAnyVarPrimF (PLitF _) = K mempty
fAnyVarPrimF (PAddF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PSubF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PMultF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PDivF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PAbsF (unK -> a)) = K a
fAnyVarPrimF (PSignumF (unK -> a)) = K a
fAnyVarPrimF (PExpF (unK -> a)) = K a
fAnyVarPrimF (PSqrtF (unK -> a)) = K a
fAnyVarPrimF (PLogF (unK -> a)) = K a
fAnyVarPrimF (PGTF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PGEF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PLTF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PLEF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PEQF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PNEQF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PAndF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (POrF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PJustF (unK -> a)) = K a
fAnyVarPrimF PNothingF = K mempty
fAnyVarPrimF (PFromJustF (unK -> a)) = K a
fAnyVarPrimF (PIsJustF (unK -> a)) = K a
fAnyVarPrimF (PPairF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PFstF (unK -> a)) = K a
fAnyVarPrimF (PSndF (unK -> a)) = K a
fAnyVarPrimF (PLengthF (unK -> a)) = K a
fAnyVarPrimF (PIndexF (unK -> a) (unK -> idx)) = K $ a <> idx
fAnyVarPrimF (PSliceF (unK -> a) (unK -> start) (unK -> end)) = K $ a <> start <> end
fAnyVarPrimF (PVecLitF (foldMap id . map unK -> as)) = K $ as
fAnyVarPrimF (PConcatF (unK -> a) (unK -> b)) = K $ a <> b

fvPrimFM ::
  FreshM m =>
  PrimF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvPrimFM (PLitF _) = K $ pure mempty
fvPrimFM (PAddF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PSubF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PMultF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PDivF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PAbsF (unK -> a)) = K a
fvPrimFM (PSignumF (unK -> a)) = K a
fvPrimFM (PExpF (unK -> a)) = K a
fvPrimFM (PSqrtF (unK -> a)) = K a
fvPrimFM (PLogF (unK -> a)) = K a
fvPrimFM (PGTF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PGEF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PLTF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PLEF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PEQF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PNEQF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PAndF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (POrF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PJustF (unK -> a)) = K a
fvPrimFM PNothingF = K $ pure mempty
fvPrimFM (PFromJustF (unK -> a)) = K a
fvPrimFM (PIsJustF (unK -> a)) = K a
fvPrimFM (PPairF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PFstF (unK -> a)) = K a
fvPrimFM (PSndF (unK -> a)) = K a
fvPrimFM (PLengthF (unK -> a)) = K a
fvPrimFM (PIndexF (unK -> a) (unK -> idx)) = K $ S.union <$> a <*> idx
fvPrimFM (PSliceF (unK -> a) (unK -> start) (unK -> end)) =
  K $ S.union <$> a <*> (S.union <$> start <*> end)
fvPrimFM (PVecLitF (sequence . map unK -> as)) = K $ foldMap id <$> as
fvPrimFM (PConcatF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b

namedPrimFMBinop ::
  forall (c :: * -> Constraint) i o m.
  (MonadThrowWithStack m, FreshM m, Typeable i, Typeable o, c i) =>
  m AnyNCPSFuzz ->
  m AnyNCPSFuzz ->
  (HFix NCPSFuzzF i -> HFix NCPSFuzzF i -> HFix NCPSFuzzF o) ->
  m AnyNCPSFuzz
namedPrimFMBinop a b comb = do
  a' <- a
  b' <- b
  case (a', b') of
    ( AnyNCPSFuzz (a' :: _ a),
      AnyNCPSFuzz (b' :: _ b)
      ) ->
        withHRefl @i @a $ \HRefl ->
          withHRefl @i @b $ \HRefl ->
            return . AnyNCPSFuzz $ comb a' b'

namedPrimFM3 ::
  forall (constraint :: * -> Constraint) i1 i2 i3 o m.
  (MonadThrowWithStack m, FreshM m, Typeable i1, Typeable i2, Typeable i3, Typeable o, constraint i1, constraint i2, constraint i3) =>
  m AnyNCPSFuzz ->
  m AnyNCPSFuzz ->
  m AnyNCPSFuzz ->
  (HFix NCPSFuzzF i1 -> HFix NCPSFuzzF i2 -> HFix NCPSFuzzF i3 -> HFix NCPSFuzzF o) ->
  m AnyNCPSFuzz
namedPrimFM3 a b c comb = do
  a' <- a
  b' <- b
  c' <- c
  case (a', b', c') of
    ( AnyNCPSFuzz (a' :: _ a),
      AnyNCPSFuzz (b' :: _ b),
      AnyNCPSFuzz (c' :: _ c)
      ) ->
        withHRefl @i1 @a $ \HRefl ->
          withHRefl @i2 @b $ \HRefl ->
            withHRefl @i3 @c $ \HRefl ->
            return . AnyNCPSFuzz $ comb a' b' c'

namedPrimFMBinop2 ::
  forall (c :: * -> Constraint) i1 i2 o m.
  (MonadThrowWithStack m, FreshM m, Typeable i1, Typeable i2, Typeable o, c i1, c i2) =>
  m AnyNCPSFuzz ->
  m AnyNCPSFuzz ->
  (HFix NCPSFuzzF i1 -> HFix NCPSFuzzF i2 -> HFix NCPSFuzzF o) ->
  m AnyNCPSFuzz
namedPrimFMBinop2 a b comb = do
  a' <- a
  b' <- b
  case (a', b') of
    ( AnyNCPSFuzz (a' :: _ a),
      AnyNCPSFuzz (b' :: _ b)
      ) ->
        withHRefl @i1 @a $ \HRefl ->
          withHRefl @i2 @b $ \HRefl ->
            return . AnyNCPSFuzz $ comb a' b'

namedPrimFMUnop ::
  forall (c :: * -> Constraint) i o m.
  (MonadThrowWithStack m, FreshM m, Typeable i, Typeable o, c i) =>
  m AnyNCPSFuzz ->
  (HFix NCPSFuzzF i -> HFix NCPSFuzzF o) ->
  m AnyNCPSFuzz
namedPrimFMUnop a comb = do
  a' <- a
  case a' of
    AnyNCPSFuzz (a' :: _ a) ->
      withHRefl @i @a $ \HRefl ->
        return . AnyNCPSFuzz $ comb a'

namedPrimFM ::
  forall a m.
  (MonadThrowWithStack m, FreshM m) =>
  PrimF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedPrimFM (PLitF x) = K . return . AnyNCPSFuzz . wrap . hinject' $ PLitF x
namedPrimFM (PAddF (unK -> a) (unK -> b)) = K
  $ namedPrimFMBinop @Num @a a b
  $ \a b -> wrap . hinject' $ PAddF a b
namedPrimFM (PSubF (unK -> a) (unK -> b)) = K
  $ namedPrimFMBinop @Num @a a b
  $ \a b -> wrap . hinject' $ PSubF a b
namedPrimFM (PMultF (unK -> a) (unK -> b)) = K
  $ namedPrimFMBinop @Num @a a b
  $ \a b -> wrap . hinject' $ PMultF a b
namedPrimFM (PDivF (unK -> a) (unK -> b)) = K
  $ namedPrimFMBinop @Fractional @a a b
  $ \a b -> wrap . hinject' $ PDivF a b
namedPrimFM (PAbsF (unK -> a)) =
  K
    $ namedPrimFMUnop @Num @a a
    $ wrap . hinject' . PAbsF
namedPrimFM (PSignumF (unK -> a)) =
  K
    $ namedPrimFMUnop @Num @a a
    $ wrap . hinject' . PSignumF
namedPrimFM (PExpF (unK -> a)) =
  K
    $ namedPrimFMUnop @Floating @a a
    $ wrap . hinject' . PExpF
namedPrimFM (PSqrtF (unK -> a)) =
  K
    $ namedPrimFMUnop @Floating @a a
    $ wrap . hinject' . PSqrtF
namedPrimFM (PLogF (unK -> a)) =
  K
    $ namedPrimFMUnop @Floating @a a
    $ wrap . hinject' . PLogF
namedPrimFM (PGTF ((unK -> a) :: _ p) (unK -> b)) = K
  $ namedPrimFMBinop @Ord @p a b
  $ \a b -> wrap . hinject' $ PGTF a b
namedPrimFM (PGEF ((unK -> a) :: _ p) (unK -> b)) = K
  $ namedPrimFMBinop @Ord @p a b
  $ \a b -> wrap . hinject' $ PGEF a b
namedPrimFM (PLTF ((unK -> a) :: _ p) (unK -> b)) = K
  $ namedPrimFMBinop @Ord @p a b
  $ \a b -> wrap . hinject' $ PLTF a b
namedPrimFM (PLEF ((unK -> a) :: _ p) (unK -> b)) = K
  $ namedPrimFMBinop @Ord @p a b
  $ \a b -> wrap . hinject' $ PLEF a b
namedPrimFM (PEQF ((unK -> a) :: _ p) (unK -> b)) = K
  $ namedPrimFMBinop @Ord @p a b
  $ \a b -> wrap . hinject' $ PEQF a b
namedPrimFM (PNEQF ((unK -> a) :: _ p) (unK -> b)) = K
  $ namedPrimFMBinop @Ord @p a b
  $ \a b -> wrap . hinject' $ PNEQF a b
namedPrimFM (PAndF (unK -> a) (unK -> b)) = K
  $ namedPrimFMBinop @Typeable @Bool a b
  $ \a b -> wrap . hinject' $ PAndF a b
namedPrimFM (POrF (unK -> a) (unK -> b)) = K
  $ namedPrimFMBinop @Typeable @Bool a b
  $ \a b -> wrap . hinject' $ POrF a b
namedPrimFM (PJustF ((unK -> a) :: _ p)) =
  K
    $ namedPrimFMUnop @Typeable @p a
    $ wrap . hinject' . PJustF
namedPrimFM PNothingF = K . return . AnyNCPSFuzz @a . wrap . hinject' $ PNothingF
namedPrimFM (PFromJustF ((unK -> a) :: _ (Maybe p))) =
  K
    $ namedPrimFMUnop @Typeable @(Maybe p) @p a
    $ wrap . hinject' . PFromJustF
namedPrimFM (PIsJustF ((unK -> a) :: _ (Maybe p))) =
  K
    $ namedPrimFMUnop @Typeable @(Maybe p) a
    $ wrap . hinject' . PIsJustF
namedPrimFM (PPairF ((unK -> a) :: _ p1) ((unK -> b) :: _ p2)) = K
  $ namedPrimFMBinop2 @Typeable @p1 @p2 a b
  $ \a b -> wrap . hinject' $ PPairF a b
namedPrimFM (PFstF ((unK -> a) :: _ (p1, p2))) =
  K
    $ namedPrimFMUnop @Typeable @(p1, p2) a
    $ wrap . hinject' . PFstF
namedPrimFM (PSndF ((unK -> a) :: _ (p1, p2))) =
  K
    $ namedPrimFMUnop @Typeable @(p1, p2) a
    $ wrap . hinject' . PSndF
namedPrimFM (PLengthF ((unK -> a) :: _ (Vec a1))) =
  K $ namedPrimFMUnop @Typeable @(Vec a1) a
    $ wrap . hinject' . PLengthF
namedPrimFM (PIndexF ((unK -> a) :: _ (Vec a1)) ((unK -> idx) :: _ Int)) = K
  $ namedPrimFMBinop2 @Typeable @(Vec a1) @Int a idx
  $ \a idx -> wrap . hinject' $ PIndexF a idx
namedPrimFM (PSliceF ((unK -> a) :: _ (Vec a1)) (unK -> start) (unK -> end)) = K
  $ namedPrimFM3 @Typeable @(Vec a1) @Int @Int a start end
  $ \a start end -> wrap . hinject' $ PSliceF a start end
namedPrimFM (PVecLitF ((sequence . map unK -> as) :: [K _ a1])) = K $ do
  as' <- as
  typedAs <- traverse (unwrap @a1) as'
  return . AnyNCPSFuzz . wrap . hinject' $ PVecLitF typedAs
  where unwrap :: forall a. Typeable a => AnyNCPSFuzz -> m (HFix NCPSFuzzF a)
        unwrap term =
          case term of
            AnyNCPSFuzz (term' :: _ a') ->
              withHRefl @a @a' $ \HRefl -> return term'
namedPrimFM (PConcatF ((unK -> a) :: _ vec) (unK -> b)) =
  K $ namedPrimFMBinop @Typeable @vec a b
    $ \a b -> wrap . hinject' $ PConcatF a b

fvCPSFuzzF ::
  FreshM m =>
  CPSFuzzF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvCPSFuzzF =
  fvBagOpFM
    `sumAlg` fvXExprMonadF
    `sumAlg` fvXExprF
    `sumAlg` fvControlFM
    `sumAlg` fvPrimFM

namedF ::
  (MonadThrowWithStack m, FreshM m) =>
  CPSFuzzF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedF =
  namedBagOpFM
    `sumAlg` namedXExprMonadFM
    `sumAlg` namedXExprFM
    `sumAlg` namedControlFM
    `sumAlg` namedPrimFM

fvCPSFuzz :: (forall f. CPSFuzz f a) -> S.Set UniqueName
fvCPSFuzz = flip evalState emptyNameState . unK . hxcata fvCPSFuzzF

named' ::
  forall a m.
  (Typeable a, FreshM m, MonadThrowWithStack m) =>
  (forall f. CPSFuzz f a) ->
  m (HFix NCPSFuzzF a)
named' term = do
  term' <- unK $ hxcata namedF term
  case term' of
    AnyNCPSFuzz (term' :: _ a') ->
      withHRefl @a @a' $ \HRefl ->
        return term'

namedEither :: Typeable a => (forall f. CPSFuzz f a) -> Either SomeException (HFix NCPSFuzzF a)
namedEither term = flip evalStateT names $ named' term
  where
    names = nameState $ fvCPSFuzz term

-- | Open up a lambda, giving access to its bound variable and the lambda's
-- body. Uses the actual bound variable when possible, otherwise generates a
-- fresh name from the name hint.
openM ::
  forall h a b m.
  ( MonadThrowWithStack m,
    HTraversable h,
    HInject ExprF h,
    Typeable a,
    Typeable b
  ) =>
  HFix h (a -> b) ->
  m (Var a, HFix h b)
openM (hproject' @ExprF . unwrap -> Just (ELamF bound body)) = return (bound, body)
openM (hproject' @ExprF . unwrap -> Just (EAppF lam2 arg)) = do
  (lam2Bound, lam2Body) <- openM lam2
  let lam2Body' = hcata' (substGenF lam2Bound arg) lam2Body
  openM lam2Body'
openM _ = throwM' UnOpenableTerm

close ::
  forall h a b.
  ( HInject ExprF h,
    Typeable a,
    Typeable b
  ) =>
  Var a ->
  HFix h b ->
  HFix h (a -> b)
close var body = wrap . hinject' $ ELamF var body

data Progress a
  = Worked a
  | Done a
  deriving (Functor)

unprogress :: Progress a -> a
unprogress (Worked a) = a
unprogress (Done a) = a

etaReduceF ::
  forall h a.
  HInject ExprF h =>
  h (HFix h) a ->
  Progress (h (HFix h) a)
etaReduceF term@(hproject' -> Just (ELamF (bound :: _ a1) body)) =
  case hproject' . unwrap $ body of
    Just (EAppF f arg) ->
      case hproject' . unwrap $ arg of
        Just (EVarF (somevar :: _ a2)) ->
          case eqTypeRep (typeRep @a1) (typeRep @a2) of
            Just HRefl ->
              if bound == somevar
                then Worked $ unwrap f
                else Done term
            _ -> Done term
        _ -> Done term
    _ -> Done term
etaReduceF term = Done term

betaReduceF ::
  forall h a.
  ( HInject ExprF h,
    --forall a. Show (HFix h a),
    HFunctor h
  ) =>
  h (HFix h) a ->
  Progress (h (HFix h) a)
betaReduceF term@(hproject' -> Just (EAppF f arg)) =
  case hproject' . unwrap $ f of
    Just (ELamF bound body) ->
      {-
            traceShow bound $
            traceShow body $
            traceShow arg $
      -}
      let result = substGen bound arg body
       in --traceShow result $
          Worked . unwrap $ result
    _ -> Done term
betaReduceF term = Done term

etaBetaReduceF ::
  forall h a.
  ( HInject ExprF h,
    HFunctor h
    --forall a. Show (HFix h a)
  ) =>
  h (Compose Progress (HFix h)) a ->
  Compose Progress (HFix h) a
etaBetaReduceF term =
  case etaReduceF . hmap (unprogress . getCompose) $ term of
    Worked t -> Compose . Worked . wrap . unprogress $ betaReduceF t
    Done t -> Compose . fmap wrap $ betaReduceF t

untilConvergence :: (a -> Progress a) -> (a -> a)
untilConvergence f a =
  case f a of
    Worked a -> untilConvergence f a
    Done a -> a

etaBetaReduceStep ::
  forall h a.
  ( HInject ExprF h,
    HFunctor h
    --forall a. Show (HFix h a)
  ) =>
  HFix h a ->
  Progress (HFix h a)
etaBetaReduceStep = getCompose . hcata' etaBetaReduceF

-- | Eta-beta reduces until convergence.
etaBetaReduce ::
  forall h a.
  ( HInject ExprF h,
    HFunctor h
    --forall a. Show (HFix h a)
  ) =>
  HFix h a ->
  HFix h a
etaBetaReduce = untilConvergence etaBetaReduceStep

monadReduceLeftF ::
  forall h a.
  ( HInject ExprMonadF h,
    HInject ExprF h,
    HFunctor h
  ) =>
  h (HFix h) a ->
  Progress (h (HFix h) a)
monadReduceLeftF term@(hproject' -> Just (EBindF m bound f)) =
  case hproject' . unwrap $ m of
    Just (EReturnF a) -> Worked . unwrap $ substGen bound a f
    _ -> Done term
monadReduceLeftF term = Done term

monadReduceRightF ::
  forall h a.
  ( HInject ExprMonadF h,
    HInject ExprF h
  ) =>
  h (HFix h) a ->
  Progress (h (HFix h) a)
monadReduceRightF term@(hproject' -> Just (EBindF m (bound :: _ a1) f)) =
  case hproject' . unwrap $ f of
    Just (EReturnF v) ->
      case hproject' . unwrap $ v of
        Just (EVarF (bound' :: _ a2)) ->
          case eqTypeRep (typeRep @a1) (typeRep @a2) of
            Just HRefl ->
              if bound == bound'
                then Worked $ unwrap m
                else Done term
            _ -> Done term
        _ -> Done term
    _ -> Done term
monadReduceRightF term = Done term

monadReduceF ::
  forall h a.
  ( HFunctor h,
    HInject ExprMonadF h,
    HInject ExprF h
  ) =>
  h (Compose Progress (HFix h)) a ->
  Compose Progress (HFix h) a
monadReduceF term =
  case monadReduceLeftF . hmap (unprogress . getCompose) $ term of
    Worked t -> Compose . Worked . wrap . unprogress $ monadReduceRightF t
    Done t -> Compose . fmap wrap $ monadReduceRightF t

monadReduceStep ::
  forall h a.
  ( HFunctor h,
    HInject ExprMonadF h,
    HInject ExprF h
  ) =>
  HFix h a ->
  Progress (HFix h a)
monadReduceStep = getCompose . hcata' monadReduceF

monadReduce ::
  forall h a.
  ( HFunctor h,
    HInject ExprMonadF h,
    HInject ExprF h
  ) =>
  HFix h a ->
  HFix h a
monadReduce = untilConvergence monadReduceStep

{-
swapGenF ::
  ( HInject ExprF h,
    HInject ExprMonadF h
  ) =>
  String ->
  String ->
  h (HFix h) a ->
  HFix h a
swapGenF var1 var2 (hproject' -> Just (EVarF (Var x)))
  | var1 == x = wrap . hinject' $ EVarF (Var var2)
swapGenF var1 var2 (hproject' -> Just (ELamF (Var bound) body))
  | var1 == bound = wrap . hinject' $ ELamF (Var var2) body
swapGenF var1 var2 (hproject' -> Just (EBindF m (Var bound) k))
  | var1 == bound = wrap . hinject' $ EBindF m (Var var2) k
swapGenF _ _ term = wrap term

swapGen ::
  ( HFunctor h,
    HInject ExprF h,
    HInject ExprMonadF h
  ) =>
  String ->
  String ->
  HFix h a ->
  HFix h a
swapGen var1 var2 = hcata' (swapGenF var1 var2)
-}

fAnyVarExprF ::
  ExprF (K (S.Set AnyVar)) a ->
  K (S.Set AnyVar) a
fAnyVarExprF (EVarF v) = K (S.singleton (AnyVar v))
fAnyVarExprF (ELamF v (unK -> body)) = K (S.delete (AnyVar v) body)
fAnyVarExprF (EAppF (unK -> a) (unK -> b)) = K (a <> b)
fAnyVarExprF (ECompF (unK -> bc) (unK -> ab)) = K (bc <> ab)

fvExprF ::
  ExprF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvExprF (EVarF (Var x)) = K (S.singleton x)
fvExprF (ELamF (Var x) (unK -> body)) = K (S.delete x body)
fvExprF (EAppF (unK -> a) (unK -> b)) = K (a <> b)
fvExprF (ECompF (unK -> bc) (unK -> ab)) = K (bc <> ab)

-- | Simple substitution that may capture.
substExprF ::
  forall a b h.
  (Typeable a, HInject ExprF h) =>
  Var a ->
  HFix h a ->
  ExprF (HFix h) b ->
  HFix h b
substExprF (Var x) u (EVarF (Var x')) =
  if x == x'
    then case eqTypeRep (typeRep @a) (typeRep @b) of
      Just HRefl ->
        {-trace "success" $-} u
      _ ->
        --trace (printf "type mismatch: %s %s" (show (typeRep @a)) (show (typeRep @b)) ) $
        wrap . hinject' $ EVarF (Var x')
    else--trace (printf "name mismatch: %s %s" x x') $
      wrap . hinject' $ EVarF (Var x')
substExprF _ _ t = wrap . hinject' $ t

substGenF ::
  (Typeable a, HInject ExprF h) =>
  Var a ->
  HFix h a ->
  h (HFix h) b ->
  HFix h b
substGenF v u term =
  case hproject' @ExprF term of
    Just expr ->
      {-trace "found expr" $ -} substExprF v u expr
    _ -> {-trace "nope" $ -} wrap term

substGen ::
  (Typeable a, HInject ExprF h, HFunctor h) =>
  Var a ->
  HFix h a ->
  HFix h b ->
  HFix h b
substGen v u term = hcata' (substGenF v u) term

substF ::
  Typeable a =>
  Var a ->
  HFix NCPSFuzzF a ->
  NCPSFuzzF (HFix NCPSFuzzF) b ->
  HFix NCPSFuzzF b
substF v u =
  recurse
    `sumAlg` recurse
    `sumAlg` substExprF v u
    `sumAlg` recurse
    `sumAlg` recurse
  where
    recurse :: HInject h NCPSFuzzF => h (HFix NCPSFuzzF) a -> HFix NCPSFuzzF a
    recurse = wrap . hinject'

flattenF ::
  forall rest h h' a m.
  ( HInject BagOpF h,
    HInject ExprF h',
    HInject FlatBagOpF h',
    HTraversable h',
    MonadThrowWithStack m,
    (BagOpF :+: rest) ~ h,
    (FlatBagOpF :+: rest) ~ h',
    HInject rest (BagOpF :+: rest),
    HInject rest h',
    FreshM m
  ) =>
  h (HFix h') a ->
  m (HFix h' a)
flattenF (hproject' -> Just (BMapF mapFun inputDb kont)) =
  case hproject' @ExprF . unwrap $ inputDb of
    Just (EVarF var) -> return . wrap . hinject' $ FBMapF mapFun var kont
    _ -> case hproject' @FlatBagOpF . unwrap $ inputDb of
      Just (FBMapF mf upperDb upperKont) -> do
        result <- gfresh "upper_bmap_result"
        -- A variable that represents the result of the upper bmap
        let var = Var result
        let newKont =
              ( wrap . hinject'
                  $ ELamF var
                  $ wrap . hinject'
                  $ FBMapF mapFun var kont
              )
                `compose` upperKont
        return . wrap . hinject' $ FBMapF mf upperDb newKont
      Just (FBSumF clip upperDb upperKont) -> do
        result <- gfresh "upper_bmap_result"
        -- A variable that represents the result of the upper bmap
        let var = Var result
        let newKont =
              ( wrap . hinject'
                  $ ELamF var
                  $ wrap . hinject'
                  $ FBMapF mapFun var kont
              )
                `compose` upperKont
        return . wrap . hinject' $ FBSumF clip upperDb newKont
      _ -> throwM' UnFlattenableTerm
flattenF (hproject' -> Just (BSumF clip inputDb kont)) =
  case hproject' @ExprF . unwrap $ inputDb of
    Just (EVarF var) -> return . wrap . hinject' $ FBSumF clip var kont
    _ -> case hproject' @FlatBagOpF . unwrap $ inputDb of
      Just (FBMapF mf upperDb upperKont) -> do
        result <- gfresh "upper_bmap_result"
        -- A variable that represents the result of the upper bmap
        let var = Var result
        let newKont =
              ( wrap . hinject'
                  $ ELamF var
                  $ wrap . hinject'
                  $ FBSumF clip var kont
              )
                `compose` upperKont
        return . wrap . hinject' $ FBMapF mf upperDb newKont
      Just (FBSumF clip upperDb upperKont) -> do
        result <- gfresh "upper_bmap_result"
        -- A variable that represents the result of the upper bmap
        let var = Var result
        let newKont =
              ( wrap . hinject'
                  $ ELamF var
                  $ wrap . hinject'
                  $ FBSumF clip var kont
              )
                `compose` upperKont
        return . wrap . hinject' $ FBSumF clip upperDb newKont
      _ -> throwM' UnFlattenableTerm
flattenF (hproject' @rest -> Just term) = return . wrap . hinject' $ term
flattenF _ = throwM' UnFlattenableTerm

flatten :: (MonadThrowWithStack m, FreshM m) => HFix NCPSFuzzF a -> m (HFix NNormalizedF a)
flatten = hcataM' flattenF

fvMainF :: MainF (K (S.Set UniqueName)) a -> K (S.Set UniqueName) a
fvMainF =
  fvExprMonadF
    `sumAlg` fvExprF
    `sumAlg` fvControlF
    `sumAlg` fvPrimF

-- ##################
-- # INFRASTRUCTURE #
-- ##################

vecConcat :: Vec a -> Vec a -> Vec a
vecConcat (Vec as) (Vec bs) = Vec (as ++ bs)

instance VecStorable () where
  vecSize = 0
  fromVec _ = ()
  asVec _ = Vec []

instance VecStorable Number where
  vecSize = 1
  fromVec (Vec (x : _)) = x
  fromVec _ = error "fromVec<Number>: cannot restore from empty vec"
  asVec a = Vec [a]

instance (VecStorable a, VecStorable b) => VecStorable (a, b) where
  vecSize = vecSize @a + vecSize @b
  fromVec (Vec as) =
    (fromVec (Vec $ take (vecSize @a) as), fromVec (Vec . take (vecSize @b) . drop (vecSize @a) $ as))
  asVec (a, b) = vecConcat (asVec a) (asVec b)

instance VecMonoid a => VecStorable (Maybe a) where
  vecSize = 1 + vecSize @a
  fromVec (Vec []) = error "fromVec: cannot decode (Maybe a) from empty vector"
  fromVec (Vec (x : more)) =
    if x > 0
      then Just (fromVec (Vec more))
      else Nothing
  asVec (Just a) =
    let Vec a' = asVec a
     in Vec (1 : a')
  asVec Nothing =
    let Vec a' = asVec (empty @a)
     in Vec (0 : a')

instance VecMonoid Number where
  add = (+)
  empty = 0

instance (VecMonoid a, VecMonoid b) => VecMonoid (a, b) where
  add (a1, b1) (a2, b2) = (add a1 a2, add b1 b2)
  empty = (empty @a, empty @b)

instance VecMonoid a => VecMonoid (Maybe a) where
  add (Just a) (Just b) = Just (a `add` b)
  add (Just a) Nothing = Just a
  add Nothing (Just b) = Just b
  add Nothing Nothing = Nothing
  empty = Nothing

instance Clip Number where
  clip (Vec [r]) a =
    if a >= bound then bound else if a <= - bound then - bound else a
    where
      bound = abs r
  clip _ _ = error "clip @Number: expect bound to have size 1"

instance (Clip a, Clip b) => Clip (a, b) where
  clip r (a, b) = (clip r a, clip r b)

instance Clip a => Clip (Maybe a) where
  clip r = fmap (clip r)

-- | Manual and deferrable resolution of the `Clip` constraint for any typeable type.
resolveClip :: forall (a :: *). Typeable a => Maybe (Dict (Clip a))
resolveClip =
  case eqTypeRep (typeRep @Number) (typeRep @a) of
    Just HRefl -> Just Dict
    _ -> do
      case (typeRep @a) of
        App maybeMaybe (a1 :: typeRep a1)
          | SomeTypeRep maybeMaybe == SomeTypeRep (typeRep @Maybe) -> do
            HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
            a1Dict <- withTypeable a1 $ resolveClip @a1
            case eqTypeRep maybeMaybe (typeRep @Maybe) of
              Just HRefl -> withDict a1Dict $ return (Dict @(Clip (Maybe a1)))
              _ -> Nothing
        App con (a2 :: typeRep a2) -> do
          HRefl <- eqTypeRep (typeRepKind a2) (typeRepKind (typeRep @Int))
          (a2Dict :: Dict (Clip a2)) <- withTypeable a2 $ resolveClip @a2
          case con of
            App maybeT2 (a1 :: TypeRep a1)
              | SomeTypeRep maybeT2 == SomeTypeRep (typeRep @(,)) -> do
                HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
                HRefl <- eqTypeRep (maybeT2) (typeRep @(,))
                (a1Dict :: Dict (Clip a1)) <- withTypeable a1 $ resolveClip @a1
                let tupleDict = withDict a1Dict $ withDict a2Dict $ Dict @(Clip (a1, a2))
                return tupleDict
            _ -> Nothing
        _ -> Nothing

resolveVecMonoid :: forall (a :: *). Typeable a => Maybe (Dict (VecMonoid a))
resolveVecMonoid =
  case eqTypeRep (typeRep @Number) (typeRep @a) of
    Just HRefl -> Just Dict
    _ -> do
      case (typeRep @a) of
        App maybeMaybe (a1 :: typeRep a1)
          | SomeTypeRep maybeMaybe == SomeTypeRep (typeRep @Maybe) -> do
            HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
            a1Dict <- withTypeable a1 $ resolveVecMonoid @a1
            case eqTypeRep maybeMaybe (typeRep @Maybe) of
              Just HRefl -> withDict a1Dict $ return (Dict @(VecMonoid (Maybe a1)))
              _ -> Nothing
        App con (a2 :: typeRep a2) -> do
          HRefl <- eqTypeRep (typeRepKind a2) (typeRepKind (typeRep @Int))
          (a2Dict :: Dict (VecMonoid a2)) <- withTypeable a2 $ resolveVecMonoid @a2
          case con of
            App maybeT2 (a1 :: TypeRep a1)
              | SomeTypeRep maybeT2 == SomeTypeRep (typeRep @(,)) -> do
                HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
                HRefl <- eqTypeRep (maybeT2) (typeRep @(,))
                (a1Dict :: Dict (VecMonoid a1)) <- withTypeable a1 $ resolveVecMonoid @a1
                let tupleDict = withDict a1Dict $ withDict a2Dict $ Dict @(VecMonoid (a1, a2))
                return tupleDict
            _ -> Nothing
        _ -> Nothing

resolveVecStorable :: forall (a :: *). Typeable a => Maybe (Dict (VecStorable a))
resolveVecStorable =
  case eqTypeRep (typeRep @Number) (typeRep @a) of
    Just HRefl -> Just Dict
    _ -> do
      case (typeRep @a) of
        App maybeMaybe (a1 :: typeRep a1)
          | SomeTypeRep maybeMaybe == SomeTypeRep (typeRep @Maybe) -> do
            HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
            a1MonoidDict <- withTypeable a1 $ resolveVecMonoid @a1
            case eqTypeRep maybeMaybe (typeRep @Maybe) of
              Just HRefl ->
                withDict a1MonoidDict $
                  return (Dict @(VecStorable (Maybe a1)))
              _ -> Nothing
        App con (a2 :: typeRep a2) -> do
          HRefl <- eqTypeRep (typeRepKind a2) (typeRepKind (typeRep @Int))
          (a2Dict :: Dict (VecStorable a2)) <- withTypeable a2 $ resolveVecStorable @a2
          case con of
            App maybeT2 (a1 :: TypeRep a1)
              | SomeTypeRep maybeT2 == SomeTypeRep (typeRep @(,)) -> do
                HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
                HRefl <- eqTypeRep (maybeT2) (typeRep @(,))
                (a1Dict :: Dict (VecStorable a1)) <- withTypeable a1 $ resolveVecStorable @a1
                let tupleDict = withDict a1Dict $ withDict a2Dict $ Dict @(VecStorable (a1, a2))
                return tupleDict
            _ -> Nothing
        _ -> Nothing

-- Instead of using codensity to turn the language functor into a monad, we use
-- rebindable syntax. This gives more freedom to us for dealing with names.

instance Syntactic (CPSFuzz f) (CPSFuzz f a) where
  type DeepRepr (CPSFuzz f a) = a
  toDeepRepr = id
  fromDeepRepr = id

instance
  ( Typeable (DeepRepr a),
    Typeable (DeepRepr b),
    Syntactic (CPSFuzz f) a,
    Syntactic (CPSFuzz f) b,
    KnownSymbol s
  ) =>
  Syntactic (CPSFuzz f) (Name s a -> b)
  where
  type DeepRepr (Name s a -> b) = (DeepRepr a -> DeepRepr b)

  toDeepRepr f = lam @s $ toDeepRepr . f . withName . fromDeepRepr . unName
  fromDeepRepr f = fromDeepRepr . app f . toDeepRepr . unName

instance
  ( Syntactic (CPSFuzz f) a,
    Typeable (DeepRepr a)
  ) =>
  Syntactic (CPSFuzz f) (Mon (CPSFuzz f) Distr a)
  where
  type DeepRepr (Mon (CPSFuzz f) Distr a) = Distr (DeepRepr a)
  toDeepRepr (Mon m) = m (xwrap . hinject' . XEReturnF . toDeepRepr)
  fromDeepRepr m = Mon $ \k -> xwrap . hinject' $ XEBindF @"shallowX" m (k . fromDeepRepr . unName)

-- ########################
-- # Necessary injections #
-- ########################

instance HInject BagOpF (BagOpF :+: h) where
  hinject' = Inl
  hproject' (Inl a) = Just a
  hproject' _ = Nothing

instance HInject FlatBagOpF (FlatBagOpF :+: h) where
  hinject' = Inl
  hproject' (Inl a) = Just a
  hproject' _ = Nothing

instance HInject XExprMonadF (a :+: XExprMonadF :+: h) where
  hinject' = Inr . Inl
  hproject' (Inr (Inl a)) = Just a
  hproject' _ = Nothing

instance HInject ExprMonadF (a :+: ExprMonadF :+: h) where
  hinject' = Inr . Inl
  hproject' (Inr (Inl a)) = Just a
  hproject' _ = Nothing

instance HInject ExprMonadF (ExprMonadF :+: h) where
  hinject' = Inl
  hproject' (Inl a) = Just a
  hproject' _ = Nothing

instance HInject XExprF (a :+: b :+: XExprF :+: h) where
  hinject' = Inr . Inr . Inl
  hproject' (Inr (Inr (Inl a))) = Just a
  hproject' _ = Nothing

instance HInject ExprF (ExprF :+: h) where
  hinject' = Inl
  hproject' (Inl a) = Just a
  hproject' _ = Nothing

instance HInject ExprF (a :+: ExprF :+: h) where
  hinject' = Inr . Inl
  hproject' (Inr (Inl a)) = Just a
  hproject' _ = Nothing

instance HInject ExprF (a :+: b :+: ExprF :+: h) where
  hinject' = Inr . Inr . Inl
  hproject' (Inr (Inr (Inl a))) = Just a
  hproject' _ = Nothing

instance HInject ControlF (a :+: ControlF :+: h) where
  hinject' = Inr . Inl
  hproject' (Inr (Inl a)) = Just a
  hproject' _ = Nothing

instance HInject ControlF (a :+: b :+: ControlF :+: h) where
  hinject' = Inr . Inr . Inl
  hproject' (Inr (Inr (Inl a))) = Just a
  hproject' _ = Nothing

instance HInject ControlF (a :+: b :+: c :+: ControlF :+: h) where
  hinject' = Inr . Inr . Inr . Inl
  hproject' (Inr (Inr (Inr (Inl a)))) = Just a
  hproject' _ = Nothing

instance HInject PrimF (a :+: b :+: PrimF) where
  hinject' = Inr . Inr
  hproject' (Inr (Inr a)) = Just a
  hproject' _ = Nothing

instance HInject PrimF (a :+: b :+: c :+: PrimF) where
  hinject' = Inr . Inr . Inr
  hproject' (Inr (Inr (Inr a))) = Just a
  hproject' _ = Nothing

instance HInject PrimF (a :+: b :+: c :+: d :+: PrimF) where
  hinject' = Inr . Inr . Inr . Inr
  hproject' (Inr (Inr (Inr (Inr a)))) = Just a
  hproject' _ = Nothing

instance HInject NRedZoneF (a :+: MainF) where
  hinject' (Inl expr) = Inr (Inr (Inl expr))
  hinject' (Inr (Inl ctrl)) = Inr (Inr (Inr (Inl ctrl)))
  hinject' (Inr (Inr prim)) = Inr (Inr (Inr (Inr prim)))

  hproject' (Inr (Inr (Inl expr))) = Just (Inl expr)
  hproject' (Inr (Inr (Inr (Inl ctrl)))) = Just (Inr (Inl ctrl))
  hproject' (Inr (Inr (Inr (Inr prim)))) = Just (Inr (Inr prim))
  hproject' _ = Nothing

instance HInject MainF (a :+: MainF) where
  hinject' = Inr

  hproject' (Inr a) = Just a
  hproject' _ = Nothing

instance HInject (ExprF :+: ControlF :+: PrimF) MainF where
  hinject' = Inr

  hproject' (Inr a) = Just a
  hproject' _ = Nothing

instance HInject McsF (McsF :+: h) where
  hinject' = Inl

  hproject' (Inl a) = Just a
  hproject' _ = Nothing

instance HInject BmcsF (BmcsF :+: h) where
  hinject' = Inl

  hproject' (Inl a) = Just a
  hproject' _ = Nothing

-- not sure why this doesn't work
--deriving via (DeriveHInjectTrans NRedZoneF MainF NMcsF) instance (HInject NRedZoneF NMcsF)

-- #######################
-- # More infrastructure #
-- #######################

newtype IsEq h a = IsEq { isEq :: HFix h a -> Bool }

eqExprF :: HInject ExprF h => ExprF (IsEq h) a -> IsEq h a
eqExprF (EVarF var) = IsEq $ \term ->
  case hproject' @ExprF . unwrap $ term of
    Just (EVarF var') -> var == var'
    _ -> False
eqExprF (ELamF var body) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (ELamF var' body') ->
      var == var' && body `isEq` body'
    _ -> False
eqExprF (EAppF f (arg :: _ arg)) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (EAppF f' (arg' :: _ arg')) ->
      case eqTypeRep (typeRep @arg) (typeRep @arg') of
        Just HRefl ->
          f `isEq` f' && arg `isEq` arg'
        _ -> False
    _ -> False
eqExprF (ECompF g (f :: _ f)) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (ECompF g' (f' :: _ f')) ->
      case eqTypeRep (typeRep @f) (typeRep @f') of
        Just HRefl ->
          g `isEq` g' && f `isEq` f'
        _ -> False
    _ -> False

eqExprMonadF :: HInject ExprMonadF h => ExprMonadF (IsEq h) a -> IsEq h a
eqExprMonadF (EParF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (EParF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqExprMonadF (ELaplaceF w c) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (ELaplaceF w' c') -> w == w' && c `isEq` c'
    _ -> False
eqExprMonadF (EExpF scores) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (EExpF scores') -> scores `isEq` scores'
    _ -> False
eqExprMonadF (EAboveThresholdF w secret guess thresh) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (EAboveThresholdF w' secret' guess' thresh') ->
      w == w' && secret `isEq` secret' && guess `isEq` guess' && thresh `isEq` thresh'
    _ -> False
eqExprMonadF (EReturnF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (EReturnF a') -> a `isEq` a'
    _ -> False
eqExprMonadF (EBindF (m :: _ m) var f) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (EBindF (m' :: _ m') var' f') ->
      case eqTypeRep (typeRep @m) (typeRep @m') of
        Just HRefl -> m `isEq` m' && var == var' && f `isEq` f'
        _ -> False
    _ -> False

eqControlF :: HInject ControlF h => ControlF (IsEq h) a -> IsEq h a
eqControlF (CIfF cond a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (CIfF cond' a' b') -> cond `isEq` cond' && a `isEq` a' && b `isEq` b'
    _ -> False
eqControlF (CLoopPureF acc pred iter) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (CLoopPureF acc' pred' iter') -> acc `isEq` acc' && pred `isEq` pred' && iter `isEq` iter'
    _ -> False
eqControlF (CLoopF acc pred iter) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (CLoopF acc' pred' iter') -> acc `isEq` acc' && pred `isEq` pred' && iter `isEq` iter'
    _ -> False

eqPrimF :: HInject PrimF h => PrimF (IsEq h) a -> IsEq h a
eqPrimF (PLitF v) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PLitF v') -> v == v'
    _ -> False
eqPrimF (PAddF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PAddF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (PSubF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PSubF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (PMultF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PMultF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (PDivF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PDivF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (PAbsF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PAbsF a') -> a `isEq` a'
    _ -> False
eqPrimF (PSignumF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PSignumF a') -> a `isEq` a'
    _ -> False
eqPrimF (PExpF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PExpF a') -> a `isEq` a'
    _ -> False
eqPrimF (PSqrtF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PSqrtF a') -> a `isEq` a'
    _ -> False
eqPrimF (PLogF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PLogF a') -> a `isEq` a'
    _ -> False
eqPrimF (PGTF (a :: _ a) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PGTF (a' :: _ a') b') ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl ->
          a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False
eqPrimF (PGEF (a :: _ a) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PGEF (a' :: _ a') b') ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl ->
          a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False
eqPrimF (PLTF (a :: _ a) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PLTF (a' :: _ a') b') ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl ->
          a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False
eqPrimF (PLEF (a :: _ a) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PLEF (a' :: _ a') b') ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl ->
          a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False
eqPrimF (PEQF (a :: _ a) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PEQF (a' :: _ a') b') ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl ->
          a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False
eqPrimF (PNEQF (a :: _ a) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PNEQF (a' :: _ a') b') ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl ->
          a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False
eqPrimF (PAndF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PAndF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (POrF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (POrF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (PJustF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PJustF a') -> a `isEq` a'
    _ -> False
eqPrimF PNothingF = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just PNothingF -> True
    _ -> False
eqPrimF (PFromJustF a) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PFromJustF a') -> a `isEq` a'
    _ -> False
eqPrimF (PIsJustF (a :: _ a)) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PIsJustF (a' :: _ a')) ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl -> a `isEq` a'
        _ -> False
    _ -> False
eqPrimF (PPairF a b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PPairF a' b') -> a `isEq` a' && b `isEq` b'
    _ -> False
eqPrimF (PFstF (a :: _ a)) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PFstF (a' :: _ a')) ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl -> a `isEq` a'
        _ -> False
    _ -> False
eqPrimF (PSndF (a :: _ a)) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PSndF (a' :: _ a')) ->
      case eqTypeRep (typeRep @a) (typeRep @a') of
        Just HRefl -> a `isEq` a'
        _ -> False
    _ -> False
eqPrimF (PLengthF (a :: _ (Vec a1))) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PLengthF (a' :: _ (Vec a2))) ->
      case eqTypeRep (typeRep @a1) (typeRep @a2) of
        Just HRefl -> a `isEq` a'
        _ -> False
    _ -> False
eqPrimF (PIndexF (a :: _ (Vec a1)) (idx :: _ Int)) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PIndexF (a' :: _ (Vec a2)) idx') ->
      case eqTypeRep (typeRep @a1) (typeRep @a2) of
        Just HRefl -> a `isEq` a' && idx `isEq` idx'
        _ -> False
    _ -> False
eqPrimF (PSliceF (a :: _ (Vec a1)) start end) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PSliceF (a' :: _ (Vec a2)) start' end') ->
      case eqTypeRep (typeRep @a1) (typeRep @a2) of
        Just HRefl -> a `isEq` a' && start `isEq` start' && end `isEq` end'
        _ -> False
    _ -> False
eqPrimF (PVecLitF (as :: [_ a1])) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PVecLitF (as' :: [_ a2])) ->
      case eqTypeRep (typeRep @a1) (typeRep @a2) of
        Just HRefl -> length as == length as' && (and $ zipWith isEq as as')
        _ -> False
    _ -> False
eqPrimF (PConcatF (a :: _ vec) b) = IsEq $ \term ->
  case hproject' . unwrap $ term of
    Just (PConcatF (a' :: _ vec') b') ->
      case eqTypeRep (typeRep @vec) (typeRep @vec') of
        Just HRefl -> a `isEq` a' && b `isEq` b'
        _ -> False
    _ -> False

eqNRedZoneF ::
  (HInject ExprF h,
   HInject ControlF h,
   HInject PrimF h) => NRedZoneF (IsEq h) a -> IsEq h a
eqNRedZoneF = eqExprF `sumAlg` eqControlF `sumAlg` eqPrimF

eqNRedZone ::
  (HInject ExprF h,
   HInject ControlF h,
   HInject PrimF h) => HFix NRedZoneF a -> IsEq h a
eqNRedZone = hcata' eqNRedZoneF

instance Eq (HFix NRedZoneF a) where
  term == term' = (eqNRedZone term) `isEq` term'

newtype Hash a = Hash { hashWithSalt_ :: Int -> Int }

infixl 0 `hashWithSaltImpl`

hashWithSaltImpl :: Int -> Hash a -> Int
hashWithSaltImpl = flip hashWithSalt_

-- | Stolen from `Data.Hashable.Class`.
combineHash :: Int -> Int -> Int
combineHash h1 h2 = (h1 * 16777619) `xor` h2

hashExprMonadF :: ExprMonadF Hash a -> Hash a
hashExprMonadF (EParF a b) = Hash $ \salt ->
  0 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashExprMonadF (ELaplaceF w c) = Hash $ \salt ->
  1 `hashWithSalt` salt `hashWithSalt` w `hashWithSaltImpl` c
hashExprMonadF (EExpF scores) = Hash $ \salt ->
  2 `hashWithSalt` salt `hashWithSaltImpl` scores
hashExprMonadF (EAboveThresholdF w secret guess thresh) = Hash $ \salt ->
  3 `hashWithSalt`
  salt `hashWithSalt`
  w `hashWithSaltImpl`
  secret `hashWithSaltImpl`
  guess `hashWithSaltImpl` thresh
hashExprMonadF (EReturnF a) = Hash $ \salt ->
  4 `hashWithSalt` salt `hashWithSaltImpl` a
hashExprMonadF (EBindF m v f) = Hash $ \salt ->
  5 `hashWithSalt` salt `hashWithSaltImpl` m `hashWithSalt` v `hashWithSaltImpl` f

instance Hashable (ExprMonadF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashExprMonadF

hashExprF :: ExprF Hash a -> Hash a
hashExprF (EVarF v) = Hash $ \salt ->
  0 `hashWithSalt` hashWithSalt salt v
hashExprF (ELamF v body) = Hash $ \salt ->
  1 `hashWithSalt` salt `hashWithSalt` v `hashWithSaltImpl` body
hashExprF (EAppF f arg) = Hash $ \salt ->
  2 `hashWithSalt` salt `hashWithSaltImpl` f `hashWithSaltImpl` arg
hashExprF (ECompF g f) = Hash $ \salt ->
  3 `hashWithSalt` salt `hashWithSaltImpl` g `hashWithSaltImpl` f

instance Hashable (ExprF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashExprF

hashControlF :: ControlF Hash a -> Hash a
hashControlF (CIfF cond a b) = Hash $ \salt ->
  0 `hashWithSalt` salt `hashWithSaltImpl` cond `hashWithSaltImpl` a `hashWithSaltImpl` b
hashControlF (CLoopPureF acc pred iter) = Hash $ \salt ->
  1 `hashWithSalt` salt `hashWithSaltImpl` acc `hashWithSaltImpl` pred `hashWithSaltImpl` iter
hashControlF (CLoopF acc pred iter) = Hash $ \salt ->
  2 `hashWithSalt` salt `hashWithSaltImpl` acc `hashWithSaltImpl` pred `hashWithSaltImpl` iter

instance Hashable (ControlF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashControlF

hashPrimF :: PrimF Hash a -> Hash a
hashPrimF (PLitF v) = Hash $ \salt ->
  0 `hashWithSalt` salt `hashWithSalt` v
hashPrimF (PAddF a b) = Hash $ \salt ->
  1 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PSubF a b) = Hash $ \salt ->
  2 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PMultF a b) = Hash $ \salt ->
  3 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PDivF a b) = Hash $ \salt ->
  4 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PAbsF a) = Hash $ \salt ->
  5 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PSignumF a) = Hash $ \salt ->
  6 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PExpF a) = Hash $ \salt ->
  7 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PSqrtF a) = Hash $ \salt ->
  8 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PLogF a) = Hash $ \salt ->
  9 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PGTF a b) = Hash $ \salt ->
  10 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PGEF a b) = Hash $ \salt ->
  11 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PLTF a b) = Hash $ \salt ->
  12 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PLEF a b) = Hash $ \salt ->
  13 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PEQF a b) = Hash $ \salt ->
  14 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PNEQF a b) = Hash $ \salt ->
  15 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PAndF a b) = Hash $ \salt ->
  16 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (POrF a b) = Hash $ \salt ->
  17 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PJustF a) = Hash $ \salt ->
  18 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF PNothingF = Hash $ \salt ->
  19 `hashWithSalt` salt
hashPrimF (PFromJustF a) = Hash $ \salt ->
  20 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PIsJustF a) = Hash $ \salt ->
  21 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PPairF a b) = Hash $ \salt ->
  22 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b
hashPrimF (PFstF a) = Hash $ \salt ->
  23 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PSndF a) = Hash $ \salt ->
  24 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PLengthF a) = Hash $ \salt ->
  25 `hashWithSalt` salt `hashWithSaltImpl` a
hashPrimF (PIndexF a idx) = Hash $ \salt ->
  26 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` idx
hashPrimF (PSliceF a start end) = Hash $ \salt ->
  27 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` start `hashWithSaltImpl` end
hashPrimF (PVecLitF as) = Hash $ \salt ->
  28 `hashWithSalt` salt `go` as
  where go salt [] = salt
        go salt (x:xs) = go (salt `hashWithSaltImpl` x) xs
hashPrimF (PConcatF a b) = Hash $ \salt ->
  29 `hashWithSalt` salt `hashWithSaltImpl` a `hashWithSaltImpl` b

instance Hashable (PrimF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashPrimF

hashMcsF :: McsF Hash a -> Hash a
hashMcsF (MRunF sz clip mf rf) = Hash $ \salt ->
  0 `hashWithSalt` salt
  `hashWithSalt` sz
  `hashWithSalt` clip
  `hashWithSaltImpl` mf
  `hashWithSaltImpl` rf

instance Hashable (McsF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashMcsF

hashBmcsF :: BmcsF Hash a -> Hash a
hashBmcsF (BRunF sz clip mst mf rst rf) = Hash $ \salt ->
  0 `hashWithSalt` salt
  `hashWithSalt` sz
  `hashWithSalt` clip
  `hashWithSaltImpl` mst
  `hashWithSaltImpl` mf
  `hashWithSaltImpl` rst
  `hashWithSaltImpl` rf

instance Hashable (BmcsF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashBmcsF

hashBagOpF :: BagOpF Hash a -> Hash a
hashBagOpF (BMapF mf db kont) = Hash $ \salt ->
  0 `hashWithSalt` salt `hashWithSaltImpl` mf `hashWithSaltImpl` db `hashWithSaltImpl` kont
hashBagOpF (BSumF clip db kont) = Hash $ \salt ->
  1 `hashWithSalt` salt `hashWithSalt` clip `hashWithSaltImpl` db `hashWithSaltImpl` kont

instance Hashable (BagOpF Hash a) where
  hashWithSalt salt =
    hashWithSaltImpl salt . hashBagOpF

instance (Hashable (f Hash a), Hashable (g Hash a)) => Hashable ((f :+: g) Hash a)

instance (HFunctor h, forall a. Hashable (h Hash a)) => Hashable (HFix h a) where
  hashWithSalt salt term = hashWithSaltImpl salt (hcata' hashAlg term)
    where hashAlg term = Hash (flip hashWithSalt term)

data Literal =
  I Int
  | D Double
  | P (Literal, Literal)
  | V (Vec Number)
  | U
  deriving (Show, Eq, Ord)

class (Hashable a, Eq a) => IsLiteral a where
  toLiteral :: a -> Literal

instance IsLiteral Int where
  toLiteral = I

instance IsLiteral Double where
  toLiteral = D

instance IsLiteral () where
  toLiteral _ = U

instance IsLiteral (Vec Number) where
  toLiteral = V

instance (IsLiteral a, IsLiteral b) => IsLiteral (a, b) where
  toLiteral (a, b) = P (toLiteral a, toLiteral b)

-- ####################
-- # Lenses to expose #
-- ####################

makeLensesWith abbreviatedFields ''TypeCheckError

-- ##########
-- # Macros #
-- ##########

named :: String -> TH.PatQ
named nm =
  [p| (N $(TH.varP (TH.mkName nm)) :: Name $(TH.LitT <$> TH.strTyLit nm) _) |]
