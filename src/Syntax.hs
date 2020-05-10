{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Constraint
import Data.Functor.Compose
import Data.Kind
import Data.List (reverse)
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

class VecStorable a where
  -- | How many dimensions does it take to store `a`?
  vecSize :: Int

  fromVec :: Vec Number -> a
  asVec :: a -> Vec Number

class VecStorable a => VecMonoid a where
  -- | Law:
  -- 1. asVec (a `add` b) = (asVec a) + (asVec b) -- can distribute `asVec`
  -- 2. empty is identity for add
  -- 3. more?
  add :: a -> a -> a

  empty :: a

class VecMonoid a => Clip a where
  -- | Law:
  -- 1. asVec (clip r a) = map (clip r) (asVec a) -- can distribute `clip r`
  -- 2. clip r empty = empty
  -- 3. more?
  clip :: Vec Number -> a -> a

-- | Carries a statically determined name hint for the wrapped value.
newtype Name :: Symbol -> * -> * where
  N :: r -> Name s r

withName :: r -> Name s r
withName = N

unName :: Name s r -> r
unName (N r) = r

class Syntactic (f :: * -> *) a where
  type DeepRepr a :: *
  toDeepRepr :: a -> f (DeepRepr a)
  fromDeepRepr :: f (DeepRepr a) -> a

type Distr = Identity

type Number = Double

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

newtype Vec a = Vec [a]
  deriving (Show, Eq, Ord)
  deriving (Functor, Applicative, Monad, Foldable) via []

data Var (a :: *) where
  Var :: Typeable a => UniqueName -> Var a

data AnyVar where
  AnyVar :: Typeable a => Var a -> AnyVar

deriving instance Show (Var a)

deriving instance Eq (Var a)

deriving instance Ord (Var a)

deriving instance Show AnyVar

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

data XExprF :: (* -> *) -> * -> * where
  XEVarF :: Typeable a => UniqueName -> XExprF r a
  XELamF :: (KnownSymbol s, Typeable a, Typeable b) => (Name s (r a) -> r b) -> XExprF r (a -> b)
  XEAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> XExprF r b

data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => Var a -> ExprF r a
  ELamF :: (Typeable a, Typeable b) => Var a -> r b -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b
  ECompF :: (Typeable a, Typeable b, Typeable c) => r (b -> c) -> r (a -> b) -> ExprF r (a -> c)
  deriving (HXFunctor) via (DeriveHXFunctor ExprF)

data XExprMonadF :: (* -> *) -> * -> * where
  XEParF :: (Typeable a, Typeable b)
        => r (Distr a)
        -> r (Distr b)
        -> XExprMonadF r (Distr (a, b))
  XELaplaceF :: Number -> r Number -> XExprMonadF r (Distr Number)
  XEReturnF :: Typeable a => r a -> XExprMonadF r (Distr a)
  XEBindF ::
    (KnownSymbol s, Typeable a, Typeable b) =>
    r (Distr a) ->
    (Name s (r a) -> r (Distr b)) ->
    XExprMonadF r (Distr b)

data ExprMonadF :: (* -> *) -> * -> * where
  EParF :: (Typeable a, Typeable b)
        => r (Distr a)
        -> r (Distr b)
        -> ExprMonadF r (Distr (a, b))
  ELaplaceF :: Number -> r Number -> ExprMonadF r (Distr Number)
  EReturnF :: Typeable a => r a -> ExprMonadF r (Distr a)
  EBindF :: (Typeable a, Typeable b) => r (Distr a) -> Var a -> r (Distr b) -> ExprMonadF r (Distr b)
  deriving (HXFunctor) via (DeriveHXFunctor ExprMonadF)

-- | Primitives supported by the language.
data PrimF :: (* -> *) -> * -> * where
  -- Arithmetics related stuff.
  PLitF :: (Typeable a, Show a) => a -> PrimF r a
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
  -- Data structures.
  PJustF :: Typeable a => r a -> PrimF r (Maybe a)
  PNothingF :: Typeable a => PrimF r (Maybe a)
  PFromJustF :: Typeable a => r (Maybe a) -> PrimF r a
  PIsJustF :: Typeable a => r (Maybe a) -> PrimF r Bool
  PPairF :: (Typeable a, Typeable b) => r a -> r b -> PrimF r (a, b)
  PFstF :: (Typeable a, Typeable b) => r (a, b) -> PrimF r a
  PSndF :: (Typeable a, Typeable b) => r (a, b) -> PrimF r b
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
  CLoopF :: Typeable a => r a -> r (a -> Bool) -> r (a -> Distr a) -> ControlF r (Distr a)
  deriving (HXFunctor) via (DeriveHXFunctor ControlF)

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
      EReturnF (f -> a) -> EReturnF a
      EBindF (f -> m) bound (f -> k) -> EBindF m bound k

instance HFoldable ExprMonadF where
  hfoldMap f =
    \case
      EParF (f -> a) (f -> b) -> a <> b
      ELaplaceF _ (f -> c) -> c
      EReturnF (f -> a) -> a
      EBindF (f -> m) _ (f -> k) -> m <> k

instance HTraversable ExprMonadF where
  htraverse f =
    \case
      EParF (f -> a) (f -> b) -> EParF <$> a <*> b
      ELaplaceF w (f -> c) -> ELaplaceF <$> pure w <*> c
      EReturnF (f -> a) -> EReturnF <$> a
      EBindF (f -> m) bound (f -> k) -> EBindF <$> m <*> pure bound <*> k

instance HXFunctor XExprMonadF where
  hxmap f g =
    \case
      XEParF (f -> a) (f -> b) -> XEParF a b
      XELaplaceF c w -> XELaplaceF c (f w)
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
      CLoopF (f -> acc) (f -> cond) (f -> iter) -> CLoopF acc cond iter

instance HFoldable ControlF where
  hfoldMap f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> cond <> a <> b
      CLoopF (f -> acc) (f -> cond) (f -> iter) -> acc <> cond <> iter

instance HTraversable ControlF where
  htraverse f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> CIfF <$> cond <*> a <*> b
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
      PJustF (f -> a) -> PJustF a
      PNothingF -> PNothingF
      PFromJustF (f -> a) -> PFromJustF a
      PIsJustF (f -> a) -> PIsJustF a
      PPairF (f -> a) (f -> b) -> PPairF a b
      PFstF (f -> a) -> PFstF a
      PSndF (f -> a) -> PSndF a

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
      PJustF (f -> a) -> a
      PNothingF -> mempty
      PFromJustF (f -> a) -> a
      PIsJustF (f -> a) -> a
      PPairF (f -> a) (f -> b) -> a <> b
      PFstF (f -> a) -> a
      PSndF (f -> a) -> a

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
      PJustF (f -> a) -> PJustF <$> a
      PNothingF -> pure PNothingF
      PFromJustF (f -> a) -> PFromJustF <$> a
      PIsJustF (f -> a) -> PIsJustF <$> a
      PPairF (f -> a) (f -> b) -> PPairF <$> a <*> b
      PFstF (f -> a) -> PFstF <$> a
      PSndF (f -> a) -> PSndF <$> a

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

type MainF = ExprMonadF :+: ExprF :+: ControlF :+: PrimF

-- | CPSFuzz, but with explicit names. This is easier for compilers to handle
-- because everything is first-order.
type NCPSFuzzF = BagOpF :+: MainF

type NRedZoneF = ExprF :+: ControlF :+: PrimF

type NOrangeZoneF = ExprF :+: ControlF :+: PrimF

-- | Morally the same type as NCPSFuzzF, but guaranteed that all input databases
-- are variable names. This lifts all of the bag operations up so that all of
-- the continuations are flattened.
type NNormalizedF = FlatBagOpF :+: MainF

type NMcsF = McsF :+: MainF

type NBmcsF = BmcsF :+: MainF

type CPSFuzz f = HXFix CPSFuzzF f

type NCPSFuzz f = HXFix NCPSFuzzF f

-- ###############################
-- # LANGUAGE SMART CONSTRUCTORS #
-- ###############################

class SynOrd (f :: * -> *) where
  infix 4 %<, %<=, %>, %>=, %==, %/=

  (%<) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%<=) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%>) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%>=) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%==) :: (Typeable a, Ord a) => f a -> f a -> f Bool
  (%/=) :: (Typeable a, Ord a) => f a -> f a -> f Bool

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

lit :: (Typeable a, Show a) => a -> CPSFuzz f a
lit = xwrap . hinject' . PLitF

var ::
  Typeable a =>
  UniqueName ->
  CPSFuzz f a
var = xwrap . hinject' . XEVarF

lam ::
  forall s a b f.
  (KnownSymbol s, Typeable a, Typeable b) =>
  (Name s (CPSFuzz f a) -> CPSFuzz f b) ->
  CPSFuzz f (a -> b)
lam = xwrap . hinject' @XExprF @CPSFuzzF . XELamF

app :: (Typeable a, Typeable b) => CPSFuzz f (a -> b) -> CPSFuzz f a -> CPSFuzz f b
app f t = xwrap . hinject' @XExprF @CPSFuzzF $ XEAppF f t

share :: (Typeable a, Typeable b) => CPSFuzz f a -> CPSFuzz f (a -> b) -> CPSFuzz f b
share = flip app

lap :: Number -> CPSFuzz f Number -> CPSFuzz f (Distr Number)
lap w c = xwrap . hinject' $ XELaplaceF w c

xpar :: (Typeable a, Typeable b) => CPSFuzz f (Distr a) -> CPSFuzz f (Distr b) -> CPSFuzz f (Distr (a, b))
xpar a b = xwrap . hinject' $ XEParF a b

xpfst :: (Typeable a, Typeable b) => CPSFuzz f (a, b) -> CPSFuzz f a
xpfst a = xwrap . hinject' $ PFstF a

xpsnd :: (Typeable a, Typeable b) => CPSFuzz f (a, b) -> CPSFuzz f b
xpsnd a = xwrap . hinject' $ PSndF a

if_ :: Typeable a => CPSFuzz f Bool -> CPSFuzz f a -> CPSFuzz f a -> CPSFuzz f a
if_ cond t f = xwrap . hinject' $ CIfF cond (toDeepRepr t) (toDeepRepr f)

loop ::
  (Typeable a, KnownSymbol s) =>
  CPSFuzz f a ->
  (Name s (CPSFuzz f a) -> CPSFuzz f Bool) ->
  (Name s (CPSFuzz f a) -> CPSFuzz f (Distr a)) ->
  CPSFuzz f (Distr a)
loop acc cond iter =
  xwrap . hinject' $ CLoopF acc (toDeepRepr cond) (toDeepRepr iter)

just :: Typeable a => CPSFuzz f a -> CPSFuzz f (Maybe a)
just = xwrap . hinject' . PJustF

nothing :: Typeable a => CPSFuzz f (Maybe a)
nothing = xwrap . hinject' $ PNothingF

isJust :: Typeable a => CPSFuzz f (Maybe a) -> CPSFuzz f Bool
isJust = xwrap . hinject' . PIsJustF

fromJust :: Typeable a => CPSFuzz f (Maybe a) -> CPSFuzz f a
fromJust = xwrap . hinject' . PFromJustF

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

bmapNothing ::
  (KnownSymbol db, Typeable a, Typeable t) =>
  CPSFuzz f a ->
  CPSFuzz f (Bag (Maybe a)) ->
  (Name db (CPSFuzz f (Bag a)) -> CPSFuzz f t) ->
  CPSFuzz f t
bmapNothing def input kont =
  bmap (\(N row :: Name "maybeRow" _) -> if_ (isJust row) (fromJust row) def) input kont

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

instance (Typeable a, Num a, Show a) => Num (CPSFuzz f a) where
  a + b = xwrap . hinject' $ PAddF a b
  a * b = xwrap . hinject' $ PMultF a b
  a - b = xwrap . hinject' $ PSubF a b
  abs = xwrap . hinject' . PAbsF
  signum = xwrap . hinject' . PSignumF
  fromInteger = xwrap . hinject' . PLitF . fromInteger

instance (Typeable a, Fractional a, Show a) => Fractional (CPSFuzz f a) where
  a / b = xwrap . hinject' $ PDivF a b
  fromRational = xwrap . hinject' . PLitF . fromRational

exp :: CPSFuzz f Number -> CPSFuzz f Number
exp = xwrap . hinject' . PExpF

log :: CPSFuzz f Number -> CPSFuzz f Number
log = xwrap . hinject' . PLogF

sqrt :: CPSFuzz f Number -> CPSFuzz f Number
sqrt = xwrap . hinject' . PSqrtF

infixl 9 %@, `apply`

(%@) ::
  ( Typeable a,
    Typeable b,
    HInject ExprF h
  ) =>
  HFix h (a -> b) ->
  HFix h a ->
  HFix h b
(%@) = apply

apply ::
  ( Typeable a,
    Typeable b,
    HInject ExprF h
  ) =>
  HFix h (a -> b) ->
  HFix h a ->
  HFix h b
apply f arg = wrap . hinject' $ EAppF f arg

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

pfst ::
  forall h a b.
  ( HInject PrimF h,
    Typeable a,
    Typeable b
  ) =>
  HFix h (a, b) ->
  HFix h a
pfst = wrap . hinject' . PFstF

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
fAnyVarExprMonadF (EReturnF (unK -> fvs)) = K fvs
fAnyVarExprMonadF (EBindF (unK -> fvs1) bound (unK -> fvs2)) =
  K $
    fvs1 `S.union` S.delete (AnyVar bound) fvs2

fvExprMonadF ::
  ExprMonadF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvExprMonadF (EParF (unK -> a) (unK -> b)) = K $ a <> b
fvExprMonadF (ELaplaceF _ (unK -> fvs)) = K fvs
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
    (AnyNCPSFuzz (a' :: _ distra),
     AnyNCPSFuzz (b' :: _ distrb)) ->
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
fAnyVarControlF (CLoopF (unK -> acc) (unK -> cond) (unK -> iter)) = K $ acc <> cond <> iter

fvControlF ::
  ControlF (K (S.Set UniqueName)) a ->
  K (S.Set UniqueName) a
fvControlF (CIfF (unK -> cond) (unK -> a) (unK -> b)) = K $ cond <> a <> b
fvControlF (CLoopF (unK -> acc) (unK -> cond) (unK -> iter)) = K $ acc <> cond <> iter

fvControlFM ::
  FreshM m =>
  ControlF (K (m (S.Set UniqueName))) a ->
  K (m (S.Set UniqueName)) a
fvControlFM (CIfF (unK -> cond) (unK -> a) (unK -> b)) =
  K $ S.union <$> cond <*> (S.union <$> a <*> b)
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
fvPrimF (PJustF (unK -> a)) = K a
fvPrimF PNothingF = K mempty
fvPrimF (PFromJustF (unK -> a)) = K a
fvPrimF (PIsJustF (unK -> a)) = K a
fvPrimF (PPairF (unK -> a) (unK -> b)) = K $ a <> b
fvPrimF (PFstF (unK -> a)) = K a
fvPrimF (PSndF (unK -> a)) = K a

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
fAnyVarPrimF (PJustF (unK -> a)) = K a
fAnyVarPrimF PNothingF = K mempty
fAnyVarPrimF (PFromJustF (unK -> a)) = K a
fAnyVarPrimF (PIsJustF (unK -> a)) = K a
fAnyVarPrimF (PPairF (unK -> a) (unK -> b)) = K $ a <> b
fAnyVarPrimF (PFstF (unK -> a)) = K a
fAnyVarPrimF (PSndF (unK -> a)) = K a

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
fvPrimFM (PJustF (unK -> a)) = K a
fvPrimFM PNothingF = K $ pure mempty
fvPrimFM (PFromJustF (unK -> a)) = K a
fvPrimFM (PIsJustF (unK -> a)) = K a
fvPrimFM (PPairF (unK -> a) (unK -> b)) = K $ S.union <$> a <*> b
fvPrimFM (PFstF (unK -> a)) = K a
fvPrimFM (PSndF (unK -> a)) = K a

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
