{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

import Data.Functor.Identity
import Data.Kind
import qualified Data.Set as S
import Names
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Type.Reflection
import Data.Functor
import Data.Functor.Compose
import Data.Fix
import GHC.TypeLits
import Data.Proxy
import Control.Monad.Catch
import Control.Lens

newtype K a b = K a
  deriving (Show, Eq, Ord, Functor)

newtype DeriveHXFunctor h f a = DeriveHXFunctor (h f a)

infixl 6 :+:
infixr 9 :.:

type h :.: j = HComp h j

newtype HComp h j r a where
  HComp :: h (j r) a -> HComp h j r a

data WithName :: * -> * where
  WithName :: KnownSymbol s => Proxy s -> r -> WithName r

withName :: forall s r. KnownSymbol s => r -> WithName r
withName = WithName (Proxy @s)

data
  (f :: (* -> *) -> * -> *)
  :+:
  (g :: (* -> *) -> * -> *)
  :: (* -> *) -> * -> * where
  Inl :: f r a -> (f :+: g) r a
  Inr :: g r a -> (f :+: g) r a

unK :: K a b -> a
unK (K a) = a

data HMaybe f a where
  HJust    :: f a -> HMaybe f a
  HNothing :: HMaybe f a
  deriving HXFunctor via (DeriveHXFunctor HMaybe)

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
  hinject'  :: h r a -> j r a
  hproject' :: j r a -> (HMaybe :.: h) r a

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

-- | Takes 2 algebras and lift them to an algebra on the sum type.
sumAlg ::
  (forall a. h f a -> f a) ->
  (forall a. j f a -> f a) ->
  (forall a. (h :+: j) f a -> f a)
sumAlg alg1 _ (Inl a) = alg1 a
sumAlg _ alg2 (Inr a) = alg2 a

class Syntactic (f :: * -> *) a where
  type DeepRepr a :: *
  toDeepRepr :: a -> f (DeepRepr a)
  fromDeepRepr :: f (DeepRepr a) -> a

type Distr = Identity

type Number = Double

data XExprF :: (* -> *) -> * -> * where
  XEVarF :: Typeable a => String -> XExprF r a
  XELamF :: (Typeable a, Typeable b) => WithName (r a -> r b) -> XExprF r (a -> b)
  XEAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> XExprF r b

data Var (a :: *) where
  Var :: Typeable a => String -> Var a

data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => Var a -> ExprF r a
  ELamF :: (Typeable a, Typeable b) => Var a -> r b -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b

data ExprMonadF :: (* -> *) -> * -> * where
  ELaplaceF :: Number -> r Number -> ExprMonadF r (Distr Number)
  EReturnF :: Typeable a => r a -> ExprMonadF r (Distr a)
  EBindF ::
    (Typeable a, Typeable b) =>
    r a ->
    r (a -> Distr b) ->
    ExprMonadF r (Distr b)
  deriving HXFunctor via (DeriveHXFunctor ExprMonadF)

instance HFunctor HMaybe where
  hmap f =
    \case
      HJust a -> HJust (f a)
      HNothing -> HNothing

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
        forall m f g. Applicative m =>
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

instance HXFunctor XExprF where
  hxmap f g =
    \case
      XEVarF x -> XEVarF x
      XELamF (WithName p lam) -> XELamF . WithName p $ (f . lam . g)
      XEAppF lam arg -> XEAppF (f lam) (f arg)

instance HFunctor ExprF where
  hmap f =
    \case
      EVarF x -> EVarF x
      ELamF bound (f -> body) -> ELamF bound body
      EAppF (f -> lam) (f -> body) -> EAppF lam body

instance HFoldable ExprF where
  hfoldMap f =
    \case
      EVarF x -> mempty
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
      ELaplaceF c w -> ELaplaceF c (f w)
      EReturnF a -> EReturnF (f a)
      EBindF a k -> EBindF (f a) (f k)

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type XExpr f = HXFix XExprF f

xwrap :: h (HXFix h f) a -> HXFix h f a
xwrap = HXFix

wrap :: h (HFix h) a -> HFix h a
wrap = HFix

xplace :: f a -> HXFix h f a
xplace = Place

lam :: (Typeable a, Typeable b) => WithName (XExpr f a -> XExpr f b) -> XExpr f (a -> b)
lam t = xwrap (XELamF t)

app :: (Typeable a, Typeable b) => XExpr f (a -> b) -> XExpr f a -> XExpr f b
app f t = xwrap (XEAppF f t)

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
hcata alg (Place term) = term
hcata alg (HXFix term) = alg . go $ term
  where
    go = hmap (hcata alg)

hcataM ::
  forall h f m.
  (HTraversable h, Monad m) =>
  (forall a. h f a -> m (f a)) ->
  (forall a. HXFix h f a -> m (f a))
hcataM algM (Place term) = pure term
hcataM algM (HXFix term) = (algM =<<) . htraverse (hcataM algM) $ term

-- | Lifting functor-functors through injection.
hinject ::
  forall h j f a.
  (HFunctor h, HInject h j) =>
  (forall f. HXFix h f a) ->
  HXFix j f a
hinject = hcata (xwrap . hinject')

hproject ::
  forall h j f a.
  (HFunctor j, HInject h j) =>
  (forall f. HXFix j f a) ->
  HXFix (HMaybe :.: h) f a
hproject = hcata (xwrap . hproject')

-- ##################
-- # LANGUAGE TOOLS #
-- ##################

data AnyExpr where
  AnyExpr :: Typeable a => HFix ExprF a -> AnyExpr

data TypeCheckError =
  TypeCheckError {
  _tceExpected :: SomeTypeRep,
  _tceObserved :: SomeTypeRep
  }
  deriving (Show, Eq, Ord)

instance Exception TypeCheckError

makeLensesWith abbreviatedFields ''TypeCheckError

namedAlg :: forall a m.
  (Typeable m, FreshM m, MonadThrow m) => XExprF (K (m AnyExpr)) a -> K (m AnyExpr) a
namedAlg (XEVarF x) = K . return . AnyExpr @a . wrap . EVarF . Var $ x
namedAlg (XELamF (WithName (p :: Proxy name) (lam :: (K (m AnyExpr) a1 -> _ b1)))) = K $ do
  freshVar <- Var @a1 <$> gfresh (symbolVal p)
  body <- (unK . lam) (K . return . AnyExpr . wrap . EVarF $ freshVar)
  case body of
    AnyExpr body ->
      return . AnyExpr . wrap $ ELamF freshVar body
namedAlg (XEAppF (a :: _ fun) (b :: _ arg)) = K $ do
  a' <- unK a
  b' <- unK b
  case (a', b') of
    (AnyExpr (a' :: _ fun'), AnyExpr (b' :: _ arg')) ->
      case ( eqTypeRep (typeRep @fun) (typeRep @fun')
           , eqTypeRep (typeRep @arg) (typeRep @arg')
           ) of
        (Just HRefl, Just HRefl) ->
          return . AnyExpr . wrap $ EAppF a' b'
        (Nothing, _) ->
          throwM $ TypeCheckError
          (SomeTypeRep $ typeRep @fun)
          (SomeTypeRep $ typeRep @fun')
        _ -> throwM $ TypeCheckError
          (SomeTypeRep $ typeRep @arg)
          (SomeTypeRep $ typeRep @arg')

named :: forall a m. (Typeable m, FreshM m, MonadThrow m, Typeable a)
  => (forall f. HXFix XExprF f a) -> m (HFix ExprF a)
named term = do
  term' <- unK $ hxcata namedAlg term
  case term' of
    AnyExpr (term' :: _ a1) ->
      case eqTypeRep (typeRep @a1) (typeRep @a) of
        Just HRefl -> return term'
        _ -> throwM $ TypeCheckError
          (SomeTypeRep (typeRep @a))
          (SomeTypeRep (typeRep @a1))

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. XExpr f (Int -> Int)
example1 = toDeepRepr . withName @"abc" $ \(x :: XExpr f Int) -> x

example2 :: forall f. XExpr f Bool
example2 = xwrap (XEVarF "y")

example3 :: forall f. XExpr f (Int -> Bool)
example3 = toDeepRepr . withName @"foo" $ \(_ :: XExpr f Int) -> (example2 @f)

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance Syntactic (XExpr f) (XExpr f a) where
  type DeepRepr (XExpr f a) = a
  toDeepRepr = id
  fromDeepRepr = id

instance
  (Typeable (DeepRepr a),
   Typeable (DeepRepr b),
   Syntactic (XExpr f) a,
   Syntactic (XExpr f) b) =>
  Syntactic (XExpr f) (WithName (a -> b))
  where
  type DeepRepr (WithName (a -> b)) = (DeepRepr a -> DeepRepr b)

  toDeepRepr (WithName p f) = lam . WithName p $ toDeepRepr . f . fromDeepRepr
  fromDeepRepr f = withName @"shallowX" $ fromDeepRepr . app f . toDeepRepr

-- Everything below this commment should be in its own module.

-- ##################
-- # Pretty Printer #
-- ##################

prettyExprF ::
  ExprF (K Doc) a ->
  K Doc a
prettyExprF (EVarF (Var x)) = K $ text x
prettyExprF (ELamF (Var bound) body) = K . parens $
  text "\\" <> text bound <> text "." <+> unK body
prettyExprF (EAppF (unK -> a) (unK -> b)) = K . parens $ a <+> b

prettyExpr :: HFix ExprF a -> Doc
prettyExpr = unK . hcata prettyExprF . relax
