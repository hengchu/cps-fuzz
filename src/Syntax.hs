{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

--import Lib
--import Names

import Data.Functor.Identity
import Data.Kind
import qualified Data.Set as S
import Names
import Text.PrettyPrint.ANSI.Leijen
import Type.Reflection
import Data.Functor
import Data.Functor.Compose
import Data.Fix

newtype K a b = K a
  deriving (Show, Eq, Ord, Functor)


infixl 6 :+:
infixr 9 :.:

type h :.: j = HComp h j

newtype HComp h j r a where
  HComp :: h (j r) a -> HComp h j r a

data (f :: (* -> *) -> * -> *) :+: (g :: (* -> *) -> * -> *) :: (* -> *) -> * -> * where
  Inl :: f r a -> (f :+: g) r a
  Inr :: g r a -> (f :+: g) r a

unK :: K a b -> a
unK (K a) = a

data HMaybe f a where
  HJust    :: f a -> HMaybe f a
  HNothing :: HMaybe f a

-- | Take the fixpoint of a functor-functor.
data HFix (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
  HFix  :: h (HFix h f) a -> HFix h f a
  Place :: f a -> HFix h f a

-- | Inject one HXFunctor into another.
class
  HInject
    (h :: (* -> *) -> * -> *)
    (j :: (* -> *) -> * -> *) where
  hinject'  :: h r a -> j r a
  hproject' :: j r a -> (HMaybe :.: h) r a

class HXFunctor (h :: (* -> *) -> * -> *) where
  hxmap ::
    (forall a. f a -> g a) ->
    (forall a. g a -> f a) ->
    (forall a. h f a -> h g a)

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

data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => String -> ExprF r a
  ELamF :: (Typeable a, Typeable b) => (r a -> r b) -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b

data ExprMonadF :: (* -> *) -> * -> * where
  ELaplaceF :: Number -> r Number -> ExprMonadF r (Distr Number)
  EReturnF :: Typeable a => r a -> ExprMonadF r (Distr a)
  EBindF ::
    (Typeable a, Typeable b) =>
    r a ->
    r (a -> Distr b) ->
    ExprMonadF r (Distr b)

instance HXFunctor HMaybe where
  hxmap f _ =
    \case
      HJust a -> HJust (f a)
      HNothing -> HNothing

instance (HXFunctor f, HXFunctor g) => HXFunctor (f :.: g) where
  hxmap f g =
    \case
      HComp a -> HComp (hxmap (hxmap f g) (hxmap g f) a)

instance (HXFunctor f, HXFunctor g) => HXFunctor (f :+: g) where
  hxmap f g =
    \case
      Inl left -> Inl $ hxmap f g left
      Inr right -> Inr $ hxmap f g right

instance HXFunctor ExprF where
  hxmap f g =
    \case
      EVarF x -> EVarF x
      ELamF lam -> ELamF $ (f . lam . g)
      EAppF lam arg -> EAppF (f lam) (f arg)

instance HXFunctor ExprMonadF where
  hxmap f _ =
    \case
      ELaplaceF c w -> ELaplaceF c (f w)
      EReturnF a -> EReturnF (f a)
      EBindF a k -> EBindF (f a) (f k)

instance HInject ExprF (ExprF :+: ExprMonadF) where
  hinject' =
    \case
      EVarF x -> Inl (EVarF x)
      ELamF f -> Inl (ELamF f)
      EAppF a b -> Inl (EAppF a b)

instance HInject ExprMonadF (ExprF :+: ExprMonadF) where
  hinject' =
    \case
      ELaplaceF c w -> Inr (ELaplaceF c w)
      EReturnF a -> Inr (EReturnF a)
      EBindF a b -> Inr (EBindF a b)

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type Expr f = HFix ExprF f

type ExprM f = HFix (ExprF :+: ExprMonadF) f

wrap :: h (HFix h f) a -> HFix h f a
wrap = HFix

place :: f a -> HFix h f a
place = Place

lam :: (Typeable a, Typeable b) => (Expr f a -> Expr f b) -> Expr f (a -> b)
lam t = wrap (ELamF t)

app :: (Typeable a, Typeable b) => Expr f (a -> b) -> Expr f a -> Expr f b
app f t = wrap (EAppF f t)

-- | Catamorphism over a functor-functor. But wait a second, can't we just
-- instantiate `f` with some monad? I guess that's OK... If `a` is a monadic
-- type, then we just have layers of uncomposed monads. The result will not be a
-- monad transformer stack, as the binds of `f` do not propagate into `a`, but
-- that's OK maybe?
hcata ::
  forall h f.
  HXFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HFix h f a -> f a)
hcata _ (Place term) = term
hcata alg (HFix term) = alg . go $ term
  where
    go = hxmap (hcata alg) place

-- | Lifting functor-functors through injection.
hinject ::
  forall h j f a.
  (HXFunctor h, HInject h j) =>
  (forall f. HFix h f a) ->
  HFix j f a
hinject = hcata (wrap . hinject')

hproject ::
  forall h j f a.
  (HXFunctor j, HInject h j) =>
  (forall f. HFix j f a) ->
  HFix (HMaybe :.: h) f a
hproject = hcata (wrap . hproject')

unwrap :: (HMaybe :.: h) (Compose Maybe (HFix h f)) a -> Maybe (h (Compose Maybe (HFix h f)) a)
unwrap (HComp (HJust a)) = Just a
unwrap (HComp HNothing) = Nothing

isJustAlg :: h (Compose Maybe (HFix h f)) a -> Compose Maybe (HFix h f) a
isJustAlg = undefined

-- ##################
-- # LANGUAGE TOOLS #
-- ##################

substExprF ::
  forall r f a.
  Typeable r =>
  String ->
  (Expr f) r ->
  ExprF (Expr f) a ->
  (Expr f) a
substExprF x needle term@(EVarF y) =
  if x == y
    then case eqTypeRep (typeRep @r) (typeRep @a) of
      Just HRefl -> needle
      Nothing -> wrap term
    else wrap term
substExprF _ _ term = wrap term

substExpr :: Typeable r => String -> (Expr f) r -> (Expr (Expr f)) a -> (Expr f) a
substExpr x needle = hcata (substExprF x needle)

fvF :: FreshM m => ExprF (K (m (S.Set String))) a -> K (m (S.Set String)) a
fvF (EVarF x) = K . return $ S.singleton x
fvF (ELamF f) = K $ do
  x <- gfresh "x"
  body <- unK $ f (K $ return (S.singleton x))
  return $ S.delete x body
fvF (EAppF a b) = K $ do
  a' <- unK a
  b' <- unK b
  return (S.union a' b')

fv :: FreshM m => (forall f. ExprM f a) -> m (S.Set String)
fv = unK . hcata (fvF `sumAlg` const (K (return S.empty)))

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. Expr f (Int -> Int)
example1 = toDeepRepr $ \(x :: Expr f Int) -> x

example2 :: forall f. Expr f Bool
example2 = wrap (EVarF "y")

example3 :: forall f. Expr f (Int -> Bool)
example3 = toDeepRepr $ \(_ :: Expr f Int) -> (example2 @f)

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance Syntactic (Expr f) (Expr f a) where
  type DeepRepr (Expr f a) = a
  toDeepRepr = id
  fromDeepRepr = id

instance
  (Typeable (DeepRepr a), Typeable (DeepRepr b), Syntactic (Expr f) a, Syntactic (Expr f) b) =>
  Syntactic (Expr f) (a -> b)
  where
  type DeepRepr (a -> b) = DeepRepr a -> DeepRepr b

  toDeepRepr f = lam $ toDeepRepr . f . fromDeepRepr
  fromDeepRepr f = fromDeepRepr . app f . toDeepRepr

-- Everything below this commment should be in its own module.

-- ##################
-- # Pretty Printer #
-- ##################
prettyExprF ::
  FreshM m =>
  ExprF (K (m Doc)) a ->
  K (m Doc) a
prettyExprF (EVarF x) = K $ do
  return $ text x
prettyExprF (ELamF f) = K $ do
  x' <- gfresh "x"
  body <- unK $ f (K $ return (text x'))
  return $ text "\\" <> text x' <+> text "->" <+> body
prettyExprF (EAppF a b) = K $ do
  a' <- unK a
  b' <- unK b
  return $ parens $ a' <+> b'

prettyExpr ::
  FreshM m =>
  (forall f. Expr f a) ->
  m Doc
prettyExpr = unK . hcata prettyExprF
