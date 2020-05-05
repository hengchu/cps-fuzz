{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
import qualified Data.Set as S
import GHC.TypeLits
import HFunctor
import qualified Language.Haskell.TH as TH
import Names
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Type.Reflection
import GHC.Stack
import Data.Proxy

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
  deriving (Functor, Applicative, Monad) via []

newtype Vec a = Vec [a]
  deriving (Show, Eq, Ord)
  deriving (Functor, Applicative, Monad) via []

data Var (a :: *) where
  Var :: Typeable a => String -> Var a

data XExprF :: (* -> *) -> * -> * where
  XEVarF :: Typeable a => String -> XExprF r a
  XELamF :: (KnownSymbol s, Typeable a, Typeable b) => (Name s (r a) -> r b) -> XExprF r (a -> b)
  XEAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> XExprF r b

data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => Var a -> ExprF r a
  ELamF :: (Typeable a, Typeable b) => Var a -> r b -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b
  deriving HXFunctor via (DeriveHXFunctor ExprF)

data XExprMonadF :: (* -> *) -> * -> * where
  XELaplaceF :: Number -> r Number -> XExprMonadF r (Distr Number)
  XEReturnF :: Typeable a => r a -> XExprMonadF r (Distr a)
  XEBindF ::
    (KnownSymbol s, Typeable a, Typeable b) =>
    r (Distr a) ->
    (Name s (r a) -> r (Distr b)) ->
    XExprMonadF r (Distr b)

data ExprMonadF :: (* -> *) -> * -> * where
  ELaplaceF :: Number -> r Number -> ExprMonadF r (Distr Number)
  EReturnF :: Typeable a => r a -> ExprMonadF r (Distr a)
  EBindF :: (Typeable a, Typeable b) => r (Distr a) -> Var a -> r (Distr b) -> ExprMonadF r (Distr b)
  deriving HXFunctor via (DeriveHXFunctor ExprMonadF)

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
  deriving HXFunctor via (DeriveHXFunctor PrimF)

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
  deriving HXFunctor via (DeriveHXFunctor BagOpF)

-- | Control flow of the language.
data ControlF :: (* -> *) -> * -> * where
  CIfF :: Typeable a => r Bool -> r a -> r a -> ControlF r a
  deriving HXFunctor via (DeriveHXFunctor ControlF)

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

instance HFoldable ExprF where
  hfoldMap f =
    \case
      EVarF _ -> mempty
      ELamF _ (f -> body) -> body
      EAppF (f -> lam) (f -> body) -> lam <> body

instance HTraversable ExprF where
  htraverse f =
    \case
      EVarF x -> pure (EVarF x)
      ELamF bound (f -> mbody) -> ELamF bound <$> mbody
      EAppF (f -> lam) (f -> body) -> EAppF <$> lam <*> body

instance HFunctor ExprMonadF where
  hmap f =
    \case
      ELaplaceF w (f -> c) -> ELaplaceF w c
      EReturnF (f -> a) -> EReturnF a
      EBindF (f -> m) bound (f -> k) -> EBindF m bound k

instance HFoldable ExprMonadF where
  hfoldMap f =
    \case
      ELaplaceF _ (f -> c) -> c
      EReturnF (f -> a) -> a
      EBindF (f -> m) _ (f -> k) -> m <> k

instance HTraversable ExprMonadF where
  htraverse f =
    \case
      ELaplaceF w (f -> c) -> ELaplaceF <$> pure w <*> c
      EReturnF (f -> a) -> EReturnF <$> a
      EBindF (f -> m) bound (f -> k) -> EBindF <$> m <*> pure bound <*> k

instance HXFunctor XExprMonadF where
  hxmap f g =
    \case
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

instance HFunctor ControlF where
  hmap f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> CIfF cond a b

instance HFoldable ControlF where
  hfoldMap f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> cond <> a <> b

instance HTraversable ControlF where
  htraverse f =
    \case
      CIfF (f -> cond) (f -> a) (f -> b) -> CIfF <$> cond <*> a <*> b

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

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type CPSFuzzF = BagOpF :+: XExprMonadF :+: XExprF :+: ControlF :+: PrimF

-- | CPSFuzz, but with explicit names. This is easier for compilers to handle
-- because everything is first-order.
type NCPSFuzzF = BagOpF :+: ExprMonadF :+: ExprF :+: ControlF :+: PrimF

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
  a %< b  = xwrap . hinject' $ PLTF a b
  a %<= b = xwrap . hinject' $ PLEF a b
  a %> b  = xwrap . hinject' $ PGTF a b
  a %>= b = xwrap . hinject' $ PGEF a b
  a %== b = xwrap . hinject' $ PEQF a b
  a %/= b = xwrap . hinject' $ PNEQF a b

instance SynMonad (CPSFuzz f) Distr where
  m >>=. f = xwrap . hinject' $ XEBindF m f
  ret = xwrap . hinject' . XEReturnF

var ::
  Typeable a =>
  String ->
  CPSFuzz f a
var = xwrap . hinject' . XEVarF

lam ::
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

if_ :: Typeable a => CPSFuzz f Bool -> CPSFuzz f a -> CPSFuzz f a -> CPSFuzz f a
if_ cond t f = xwrap . hinject' $ CIfF cond (toDeepRepr t) (toDeepRepr f)

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

-- ##################
-- # LANGUAGE TOOLS #
-- ##################

data AnyNCPSFuzz where
  AnyNCPSFuzz :: Typeable a => HFix NCPSFuzzF a -> AnyNCPSFuzz

data TypeCheckError
  = TypeCheckError
      { _tceExpected :: SomeTypeRep,
        _tceObserved :: SomeTypeRep
      }
  deriving (Show, Eq, Ord)

data ExpectArrowTypeError =
  ExpectArrowTypeError { _eatObserved :: SomeTypeRep }
  deriving (Show, Eq, Ord)

data ExpectBagTypeError =
  ExpectBagTypeError { _ebtObserved :: SomeTypeRep }
  deriving (Show, Eq, Ord)

instance Exception TypeCheckError
instance Exception ExpectArrowTypeError
instance Exception ExpectBagTypeError

type MonadThrowWithStack m = (MonadThrow m, HasCallStack)

data WithCallStack exc = WithCallStack CallStack exc
  deriving (Show)

instance Exception exc => Exception (WithCallStack exc)

throwM' :: (MonadThrowWithStack m, Exception e) => e -> m a
throwM' = throwM . WithCallStack callStack

withArrowType :: forall unknown r m.
  (MonadThrowWithStack m, Typeable unknown) =>
  (forall a b. (unknown ~ (a -> b), Typeable a, Typeable b) => Proxy (a -> b) -> m r) -> m r
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
  where trUnknown = typeRep @unknown

withBagType :: forall unknown r m.
  (MonadThrowWithStack m, Typeable unknown) =>
  (forall a. (unknown ~ Bag a, Typeable a) => Proxy (Bag a) -> m r) -> m r
withBagType k =
  case trUnknown of
    App someTyCon (a :: TypeRep a) ->
      case eqTypeRep someTyCon (typeRep @Bag) of
        Just HRefl -> case eqTypeRep trUnknown (typeRep @(Bag a)) of
          Just HRefl -> withTypeable a (k Proxy)
          _ -> throwM' . ExpectBagTypeError $ (SomeTypeRep trUnknown)
        _ -> throwM' . ExpectBagTypeError $ (SomeTypeRep trUnknown)
    _ -> throwM' . ExpectBagTypeError $ (SomeTypeRep trUnknown)
  where trUnknown = typeRep @unknown

withHRefl :: forall a b m r.
  (Typeable a, Typeable b, MonadThrowWithStack m) =>
  ((a :~~: b) -> m r) -> m r
withHRefl k =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> k HRefl
    Nothing -> throwM' $ TypeCheckError (SomeTypeRep (typeRep @a)) (SomeTypeRep (typeRep @b))

fvBagOpF :: BagOpF (K (S.Set String)) a
         -> K (S.Set String) a
fvBagOpF (BMapF (unK -> f) (unK -> db) (unK -> kont)) = K $ f <> db <> kont
fvBagOpF (BSumF _ (unK -> db) (unK -> kont)) = K $ db <> kont

fvBagOpFM :: FreshM m =>
  BagOpF (K (m (S.Set String))) a ->
  K (m (S.Set String)) a
fvBagOpFM (BMapF (unK -> f) (unK -> db) (unK -> kont)) = K $ S.union <$> f <*> (S.union <$> db <*> kont)
fvBagOpFM (BSumF _ (unK -> db) (unK -> kont)) = K $ S.union <$> db <*> kont

namedBagOpFM :: (MonadThrowWithStack m, FreshM m) =>
  BagOpF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedBagOpFM (BMapF ((unK -> f) :: _ (a -> b)) ((unK -> db) :: _ (Bag a)) ((unK -> kont) :: _ (Bag b -> t))) = K $ do
  f'    <- f
  db'   <- db
  kont' <- kont
  case (f', db', kont') of
    (AnyNCPSFuzz (f' :: _ (a_arrow_b)),
     AnyNCPSFuzz (db' :: _ baga),
     AnyNCPSFuzz (kont' :: _ (bagb_arrow_t))) ->
      withHRefl @(a -> b) @a_arrow_b $ \HRefl ->
      withHRefl @(Bag a) @baga $ \HRefl ->
      withHRefl @(Bag b -> t) @bagb_arrow_t $ \HRefl ->
      return . AnyNCPSFuzz . wrap . hinject' $ BMapF f' db' kont'
namedBagOpFM (BSumF clip ((unK -> db) :: _ (Bag Number)) ((unK -> kont) :: _ (Number -> t))) = K $ do
  db' <- db
  kont' <- kont
  case (db', kont') of
    (AnyNCPSFuzz (db' :: _ bagnum),
     AnyNCPSFuzz (kont' :: _ num_arrow_t)) ->
      withHRefl @(Bag Number) @bagnum $ \HRefl ->
      withHRefl @(Number -> t) @num_arrow_t $ \HRefl ->
      return . AnyNCPSFuzz . wrap . hinject' $ BSumF clip db' kont'

fvXExprMonadF ::
  FreshM m =>
  XExprMonadF (K (m (S.Set String))) a ->
  K (m (S.Set String)) a
fvXExprMonadF (XELaplaceF _ (unK -> w)) = K w
fvXExprMonadF (XEReturnF (unK -> a)) = K a
fvXExprMonadF (XEBindF (unK -> m) f) = K $ do
  m' <- m
  x <- gfresh "x"
  f' <- unK $ f (N . K . return . S.singleton $ x)
  return $ S.union m' (S.delete x f')

namedXExprMonadFM :: (MonadThrowWithStack m, FreshM m) =>
  XExprMonadF (K (m AnyNCPSFuzz)) a ->
  K (m AnyNCPSFuzz) a
namedXExprMonadFM (XEReturnF (unK -> a)) = K $ do
  AnyNCPSFuzz a' <- a
  return . AnyNCPSFuzz . wrap . hinject' $ EReturnF a'
namedXExprMonadFM (XEBindF ((unK -> m) :: _ (Distr a)) (lam :: Name s _ -> _ (Distr b))) = K $ do
  m' <- m
  x <- gfresh (symbolVal (Proxy :: _ s))
  let var = Var @a x
  f' <- unK $ lam (N . K . return . AnyNCPSFuzz . wrap . hinject' $ EVarF var)
  case (m', f') of
    (AnyNCPSFuzz (m' :: _ distr_a),
     AnyNCPSFuzz (f' :: _ distr_b)) ->
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
  XExprF (K (m (S.Set String))) a ->
  K (m (S.Set String)) a
fvXExprF (XEVarF x) = K . return . S.singleton $ x
fvXExprF (XELamF f) = K $ do
  x <- gfresh "x"
  f' <- unK $ f (N . K . return . S.singleton $ x)
  return $ S.delete x f'
fvXExprF (XEAppF (unK -> a) (unK -> b)) =
  K $ S.union <$> a <*> b

fvControlF ::
  ControlF (K (S.Set String)) a ->
  K (S.Set String) a
fvControlF (CIfF (unK -> cond) (unK -> a) (unK -> b)) = K $ cond <> a <> b

fvControlFM ::
  FreshM m =>
  ControlF (K (m (S.Set String))) a ->
  K (m (S.Set String)) a
fvControlFM (CIfF (unK -> cond) (unK -> a) (unK -> b)) =
  K $ S.union <$> cond <*> (S.union <$> a <*> b)

fvPrimF ::
  PrimF (K (S.Set String)) a ->
  K (S.Set String) a
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

fvPrimFM ::
  FreshM m =>
  PrimF (K (m (S.Set String))) a ->
  K (m (S.Set String)) a
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

fvCPSFuzzF :: FreshM m =>
  CPSFuzzF (K (m (S.Set String))) a ->
  K (m (S.Set String)) a
fvCPSFuzzF = fvBagOpFM `sumAlg` fvXExprMonadF `sumAlg` fvXExprF `sumAlg` fvControlFM `sumAlg` fvPrimFM

fvCPSFuzz :: (forall f. CPSFuzz f a) -> S.Set String
fvCPSFuzz = flip evalState emptyNameState . unK . hxcata fvCPSFuzzF

-- ##################
-- # INFRASTRUCTURE #
-- ##################

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

-- Everything below this commment should be in its own module.

-- ##################
-- # Pretty Printer #
-- ##################

prettyExprF ::
  ExprF (K Doc) a ->
  K Doc a
prettyExprF (EVarF (Var x)) = K $ text x
prettyExprF (ELamF (Var bound) body) =
  K . parens $
    text "\\" <> text bound <> text "." <+> unK body
prettyExprF (EAppF (unK -> a) (unK -> b)) = K . parens $ a <+> b

prettyExpr :: HFix ExprF a -> Doc
prettyExpr = unK . hcata prettyExprF . relax

-- ########################
-- # Necessary injections #
-- ########################

instance HInject BagOpF (BagOpF :+: h) where
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

instance HInject XExprF (a :+: b :+: XExprF :+: h) where
  hinject' = Inr . Inr . Inl
  hproject' (Inr (Inr (Inl a))) = Just a
  hproject' _ = Nothing

instance HInject ExprF (a :+: b :+: ExprF :+: h) where
  hinject' = Inr . Inr . Inl
  hproject' (Inr (Inr (Inl a))) = Just a
  hproject' _ = Nothing

instance HInject ControlF (a :+: b :+: c :+: ControlF :+: h) where
  hinject' = Inr . Inr . Inr . Inl
  hproject' (Inr (Inr (Inr (Inl a)))) = Just a
  hproject' _ = Nothing

instance HInject PrimF (a :+: b :+: c :+: d :+: PrimF) where
  hinject' = Inr . Inr . Inr . Inr
  hproject' (Inr (Inr (Inr (Inr a)))) = Just a
  hproject' _ = Nothing

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
