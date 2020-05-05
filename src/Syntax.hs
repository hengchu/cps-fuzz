{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

import Control.Lens
import Control.Monad.Catch
import Data.Proxy
import qualified Data.Set as S
import GHC.TypeLits
import Names
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Type.Reflection
import Control.Monad.State.Strict
import qualified Language.Haskell.TH as TH

newtype K a b = K a
  deriving (Show, Eq, Ord, Functor)

newtype DeriveHXFunctor    h     f a = DeriveHXFunctor    (h f a)
newtype DeriveHInjectTrans h j l r a = DeriveHInjectTrans (h r a)

-- | This is right-associative so we can pattern match on the first type
-- parameter in typeclass instances.
infixr 6 :+:

infixr 9 :.:

type h :.: j = HComp h j

newtype HComp h j r a where
  HComp :: h (j r) a -> HComp h j r a

-- | Carries a statically determined name hint for the wrapped value.
newtype Name :: Symbol -> * -> * where
  N :: r -> Name s r

withName :: r -> Name s r
withName = N

unName :: Name s r -> r
unName (N r) = r

data
  (f :: (* -> *) -> * -> *)
    :+: (g :: (* -> *) -> * -> *) :: (* -> *) -> * -> * where
  Inl :: f r a -> (f :+: g) r a
  Inr :: g r a -> (f :+: g) r a

unK :: K a b -> a
unK (K a) = a

instance Semigroup m => Semigroup (K m a) where
  (K a) <> (K b) = K (a <> b)

instance Monoid m => Monoid (K m a) where
  mempty = K mempty

data HMaybe f a where
  HJust :: f a -> HMaybe f a
  HNothing :: HMaybe f a
  deriving (HXFunctor) via (DeriveHXFunctor HMaybe)

-- | Take the fixpoint of a functor-functor.
data HXFix (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
  HXFix :: h (HXFix h f) a -> HXFix h f a
  Place :: f a -> HXFix h f a

data HFix (h :: (* -> *) -> * -> *) (a :: *) where
  HFix :: h (HFix h) a -> HFix h a

-- | Inject one HXFunctor into another.
class
  HInject
    (h :: (* -> *) -> * -> *)
    (j :: (* -> *) -> * -> *) where
  hinject' :: h r a -> j r a
  hproject' :: j r a -> Maybe (h r a)

class HFunctor (h :: (* -> *) -> * -> *) where
  hmap ::
    (forall a. f a -> g a) ->
    (forall a. h f a -> h g a)

class HFunctor h => HFoldable h where
  hfoldMap ::
    Monoid m =>
    (forall a. f a -> m) ->
    (forall a. h f a -> m)

class HFoldable h => HTraversable h where
  htraverse ::
    Applicative m =>
    (forall a. f a -> m (g a)) ->
    (forall a. h f a -> m (h g a))

class HXFunctor (h :: (* -> *) -> * -> *) where
  hxmap ::
    (forall a. f a -> g a) ->
    (forall a. g a -> f a) ->
    (forall a. h f a -> h g a)

-- morally true, but overlaps with other instances
instance HFunctor h => HXFunctor (DeriveHXFunctor h) where
  hxmap f _ (DeriveHXFunctor term) = DeriveHXFunctor $ hmap f term

infixr 6 `sumAlg`

-- | Takes 2 algebras and lift them to an algebra on the sum type.
sumAlg ::
  (forall a. h f a -> f a) ->
  (forall a. j f a -> f a) ->
  (forall a. (h :+: j) f a -> f a)
sumAlg alg1 _ (Inl a) = alg1 a
sumAlg _ alg2 (Inr a) = alg2 a

sumAlgConst ::
  HInject h j =>
  (forall a. h f a -> f a) ->
  (forall a. f a) ->
  (forall a. j f a -> f a)
sumAlgConst alg _ (hproject' -> Just term) = alg term
sumAlgConst alg c _                        = c

-- | Lift an algebra through fully polymorphic injection.
sumAlgMonoid ::
  (forall a. Monoid (f a), HInject h j) =>
  (forall a. h f a -> f a) ->
  (forall a. j f a -> f a)
sumAlgMonoid alg (hproject' -> Just term) = alg term
sumAlgMonoid _   _                        = mempty

class Syntactic (f :: * -> *) a where
  type DeepRepr a :: *
  toDeepRepr :: a -> f (DeepRepr a)
  fromDeepRepr :: f (DeepRepr a) -> a

type Distr = Identity

type Number = Double

newtype Mon f m a = Mon { runMon :: forall b. Typeable b => (a -> f (m b)) -> f (m b) }
  deriving Functor

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
  EBindF :: (Typeable a, Typeable b) => r a -> Var a -> r (Distr b) -> ExprMonadF r (Distr b)

-- | Primitives supported by the language.
data PrimF :: (* -> *) -> * -> * where
  -- Arithmetics related stuff.
  PLitF  :: (Typeable a, Show a) => a -> PrimF r a
  PAddF  :: (Num a, Typeable a)  => r a -> r a -> PrimF r a
  PSubF  :: (Num a, Typeable a)  => r a -> r a -> PrimF r a
  PMultF :: (Num a, Typeable a)  => r a -> r a -> PrimF r a
  PDivF  :: (Fractional a, Typeable a) => r a -> r a -> PrimF r a
  PExpF  :: (Floating a, Typeable a) => r a -> PrimF r a
  PSqrtF :: (Floating a, Typeable a) => r a -> PrimF r a
  PLogF  :: (Floating a, Typeable a) => r a -> PrimF r a

  -- Data structures.
  PJustF     :: Typeable a => r a -> PrimF r (Maybe a)
  PNothingF  :: Typeable a => PrimF r (Maybe a)
  PFromJustF :: Typeable a => r (Maybe a) -> PrimF r a
  PIsJustF   :: Typeable a => r (Maybe a) -> PrimF r Bool

  PPairF :: (Typeable a, Typeable b) => r a -> r b -> PrimF r (a, b)
  PFstF  :: (Typeable a, Typeable b) => r (a, b) -> PrimF r a
  PSndF  :: (Typeable a, Typeable b) => r (a, b) -> PrimF r b

-- | Bag operations.
data BagOpF :: (* -> *) -> * -> * where
  BMapF :: r (Bag a) -> r (a -> b) -> BagOpF r (Bag b)
  --BSumF ::

-- | Control flow of the language.
data ControlF :: (* -> *) -> * -> * where
  CIfF :: Typeable a => r Bool -> r a -> r a -> ControlF r a

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

instance HXFunctor XExprMonadF where
  hxmap f g =
    \case
      XELaplaceF c w -> XELaplaceF c (f w)
      XEReturnF a    -> XEReturnF (f a)
      XEBindF a (k :: Name s _ -> _) -> XEBindF @s (f a) (f . k . withName . g . unName)

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type CPSFuzzF   = BagOpF :+: XExprMonadF :+: XExprF :+: ControlF :+: PrimF
type CPSFuzz  f = HXFix CPSFuzzF f

xwrap :: h (HXFix h f) a -> HXFix h f a
xwrap = HXFix

wrap :: h (HFix h) a -> HFix h a
wrap = HFix

xplace :: f a -> HXFix h f a
xplace = Place

lam :: (KnownSymbol s, Typeable a, Typeable b)
  => (Name s (CPSFuzz f a) -> CPSFuzz f b) -> CPSFuzz f (a -> b)
lam = xwrap . hinject' @XExprF @CPSFuzzF . XELamF

app :: (Typeable a, Typeable b) => CPSFuzz f (a -> b) -> CPSFuzz f a -> CPSFuzz f b
app f t = xwrap . hinject' @XExprF @CPSFuzzF $ XEAppF f t

lap :: Number -> CPSFuzz f Number -> CPSFuzz f (Distr Number)
lap w c = xwrap . hinject' $ XELaplaceF w c

-- | Relax a fixpoint into a fixpoint potentially with holes in it. This allows
-- us to fold over the structure with catamorphism.
relax :: HFunctor h => HFix h a -> HXFix h f a
relax (HFix term) = HXFix . (hmap relax) $ term

-- | Catamorphism over a functor-functor. But wait a second, can't we just
-- instantiate `f` with some monad? I guess that's OK... If `a` is a monadic
-- type, then we just have layers of uncomposed monads. The result will not be a
-- monad transformer stack, as the binds of `f` do not propagate into `a`, but
-- that's OK maybe?
hxcata ::
  forall h f.
  HXFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HXFix h f a -> f a)
hxcata _ (Place term) = term
hxcata alg (HXFix term) = alg . go $ term
  where
    go = hxmap (hxcata alg) xplace

hcata ::
  forall h f.
  HFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HXFix h f a -> f a)
hcata _   (Place term) = term
hcata alg (HXFix term) = alg . go $ term
  where
    go = hmap (hcata alg)

hcata' ::
  forall h f.
  HFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HFix h a -> f a)
hcata' alg (HFix term) =
  alg . hmap (hcata' alg) $ term

hcataM ::
  forall h f m.
  (HTraversable h, Monad m) =>
  (forall a. h f a -> m (f a)) ->
  (forall a. HXFix h f a -> m (f a))
hcataM _    (Place term) = pure term
hcataM algM (HXFix term) = (algM =<<) . htraverse (hcataM algM) $ term

hcataM' ::
  forall h f m.
  (HTraversable h, Monad m) =>
  (forall a. h f a -> m (f a)) ->
  (forall a. HFix h a -> m (f a))
hcataM' algM (HFix term) = (algM =<<) . htraverse (hcataM' algM) $ term

-- | Lifting functor-functors through injection.
inject ::
  forall h j f a.
  (HXFunctor h, HInject h j) =>
  (HXFix h (HXFix j f) a) ->
  HXFix j f a
inject = hxcata (xwrap . hinject')

inject' ::
  forall h j a.
  (HFunctor h, HInject h j) =>
  HFix h a ->
  HFix j a
inject' = hcata' (wrap . hinject')

hproject ::
  forall h j a.
  (HFunctor j,
   HTraversable h,
   HInject h j) =>
  HFix j a ->
  Maybe (HFix h a)
hproject =
  hcataM' (fmap wrap . unMaybeHomM)
  . hcata' (wrap . prj . hproject')
  where
    prj :: forall h r a. Maybe (h r a) -> (HMaybe :.: h) r a
    prj (Just a) = HComp (HJust a)
    prj Nothing  = HComp HNothing

unMaybeHomM :: (HMaybe :.: h) f a -> Maybe (h f a)
unMaybeHomM (HComp (HJust a)) = Just a
unMaybeHomM (HComp HNothing)  = Nothing

-- ##################
-- # LANGUAGE TOOLS #
-- ##################


class SynMonad h (m :: * -> *) where
  infixl 1 >>=.
  (>>=.) :: (KnownSymbol s, Typeable a, Typeable b)
    => h (m a) -> (Name s (h a) -> h (m b)) -> h (m b)
  ret :: Typeable a => h a -> h (m a)

instance SynMonad (CPSFuzz f) Distr where
  m >>=. f = xwrap . hinject' $ XEBindF m f
  ret = xwrap . hinject' . XEReturnF

data AnyCPSFuzz where
  AnyCPSFuzz :: Typeable a => HFix ExprF a -> AnyCPSFuzz

data TypeCheckError
  = TypeCheckError
      { _tceExpected :: SomeTypeRep,
        _tceObserved :: SomeTypeRep
      }
  deriving (Show, Eq, Ord)

instance Exception TypeCheckError

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
  (Syntactic (CPSFuzz f) a,
   Typeable (DeepRepr a)
  ) =>
  Syntactic (CPSFuzz f) (Mon (CPSFuzz f) Distr a) where
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

-- ##########################################
-- # General higher-order functor instances #
-- ##########################################

instance HFunctor HMaybe where
  hmap f =
    \case
      HJust a -> HJust (f a)
      HNothing -> HNothing

instance HFoldable HMaybe where
  hfoldMap f =
    \case
      HJust a -> f a
      HNothing -> mempty

instance HTraversable HMaybe where
  htraverse f =
    \case
      HJust a -> HJust <$> f a
      HNothing -> pure HNothing

instance HFunctor h => HFunctor (HXFix h) where
  hmap f =
    \case
      HXFix term -> HXFix (hmap (hmap f) term)
      Place term -> Place (f term)

instance HFoldable h => HFoldable (HXFix h) where
  hfoldMap = hfoldMap'
    where
      hfoldMap' ::
        forall m f. Monoid m => (forall a. f a -> m) -> (forall a. HXFix h f a -> m)
      hfoldMap' f (Place a) = f a
      hfoldMap' f (HXFix a) = hfoldMap (hfoldMap' f) a

instance HTraversable h => HTraversable (HXFix h) where
  htraverse = htraverse'
    where
      htraverse' ::
        forall m f g.
        Applicative m =>
        (forall a. f a -> m (g a)) ->
        (forall a. HXFix h f a -> m (HXFix h g a))
      htraverse' f (Place a) = Place <$> f a
      htraverse' f (HXFix a) = HXFix <$> htraverse (htraverse' f) a

instance (HFunctor f, HFunctor g) => HFunctor (f :.: g) where
  hmap f =
    \case
      HComp a -> HComp (hmap (hmap f) a)

instance (HFoldable f, HFoldable g) => HFoldable (f :.: g) where
  hfoldMap f =
    \case
      HComp a -> hfoldMap (hfoldMap f) a

instance (HTraversable f, HTraversable g) => HTraversable (f :.: g) where
  htraverse f =
    \case
      HComp a -> HComp <$> htraverse (htraverse f) a

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
  hmap f =
    \case
      Inl left -> Inl $ hmap f left
      Inr right -> Inr $ hmap f right

instance (HFoldable f, HFoldable g) => HFoldable (f :+: g) where
  hfoldMap f =
    \case
      Inl left -> hfoldMap f left
      Inr right -> hfoldMap f right

instance (HTraversable f, HTraversable g) => HTraversable (f :+: g) where
  htraverse f =
    \case
      Inl left -> Inl <$> htraverse f left
      Inr right -> Inr <$> htraverse f right

instance (HInject h j, HInject j l) => HInject (DeriveHInjectTrans h j l) l where
  hinject' (DeriveHInjectTrans a) = hinject' @j @l . hinject' @h @j $ a
  hproject' a =
    hproject' @j @l a >>= hproject' @h @j >>= (return . DeriveHInjectTrans)

instance HInject BagOpF (BagOpF :+: h) where
  hinject' = Inl
  hproject' (Inl a) = Just a
  hproject' _       = Nothing

instance HInject XExprMonadF (a :+: XExprMonadF :+: h) where
  hinject' = Inr . Inl
  hproject' (Inr (Inl a)) = Just a
  hproject' _       = Nothing

instance HInject XExprF (a :+: b :+: XExprF :+: h) where
  hinject' = Inr . Inr . Inl
  hproject' (Inr (Inr (Inl a))) = Just a
  hproject' _       = Nothing

instance HInject ControlF (a :+: b :+: c :+: ControlF :+: h) where
  hinject' = Inr . Inr . Inr . Inl
  hproject' (Inr (Inr (Inr (Inl a)))) = Just a
  hproject' _       = Nothing

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
