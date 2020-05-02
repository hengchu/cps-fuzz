{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

--import Lib
--import Names

import Data.Kind
import qualified Data.Set as S
import Type.Reflection
import Data.Functor.Identity

import Names
import Text.PrettyPrint.ANSI.Leijen

newtype K a b = K a
  deriving (Show, Eq, Ord, Functor)

infixl 6 :+:

data (f :: (* -> *) -> * -> *) :+: (g :: (* -> *) -> * -> *) :: (* -> *) -> * -> * where
  Inl :: f r a -> (f :+: g) r a
  Inr :: g r a -> (f :+: g) r a

unK :: K a b -> a
unK (K a) = a

-- | Take the fixpoint of a functor-functor.
data HFix (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
  HFix  :: h (HFix h f) a -> HFix h f a
  Place :: f a -> HFix h f a

class Inject
  (f :: (* -> *) -> * -> *)
  (g :: (* -> *) -> * -> *) where
  inject' :: f r a -> g r a

class HXFunctor (h :: (* -> *) -> * -> *) where
  hxmap ::
    (forall a. f a -> g a) ->
    (forall a. g a -> f a) ->
    (forall a. h f a -> h g a)

-- | A variant of `HXFunctor` that allows mapping over constrained data. This
-- assumes the constraint `c` can be satisfied by pattern matching on the
-- constructors of h.
class HXCFunctor (c :: * -> Constraint) (h :: (* -> *) -> * -> *) where
  hxcmap ::
    (forall a. c a => f a -> g a) ->
    (forall a. c a => g a -> f a) ->
    (forall a. h f a -> h g a)

class Syntactic (f :: * -> *) a where
  type DeepRepr a :: *
  toDeepRepr :: a -> f (DeepRepr a)
  fromDeepRepr :: f (DeepRepr a) -> a

type Distr  = Identity
type Number = Double

data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => String -> ExprF r a
  ELamF :: (Typeable a, Typeable b) => (r a -> r b) -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b

  ELaplaceF :: Number -> r Number -> ExprF r (Distr Number)
  EReturnF  :: Typeable a => r a -> ExprF r (Distr a)
  EBindF    :: (Typeable a, Typeable b) => r a -> r (a -> Distr b) -> ExprF r (Distr b)

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
      ELaplaceF c w -> ELaplaceF c (f w)
      EReturnF a -> EReturnF (f a)
      EBindF a k -> EBindF (f a) (f k)

instance (HXCFunctor c f, HXCFunctor c g) => HXCFunctor c (f :+: g) where
  hxcmap f g =
    \case
      Inl left -> Inl $ hxcmap @c f g left
      Inr right -> Inr $ hxcmap @c f g right

instance HXCFunctor Typeable ExprF where
  hxcmap f g =
    \case
      EVarF x -> EVarF x
      ELamF lam -> ELamF (f . lam . g)
      EAppF lam arg -> EAppF (f lam) (f arg)
      ELaplaceF c w -> ELaplaceF c (f w)
      EReturnF a -> EReturnF (f a)
      EBindF a k -> EBindF (f a) (f k)

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type Expr f = HFix ExprF f

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

hccata ::
  forall c h f.
  HXCFunctor c h =>
  (forall a. c a => h f a -> f a) ->
  (forall a. c a => HFix h f a -> f a)
hccata alg =
  \case
    Place term -> term
    HFix term -> alg . go $ term
  where
    go = hxcmap @c (hccata @c alg) place

-- ##################
-- # LANGUAGE TOOLS #
-- ##################

substExprF ::
  forall r f a.
  (Typeable r, Typeable a) =>
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

substExpr :: (Typeable r, Typeable a) => String -> (Expr f) r -> (Expr (Expr f)) a -> (Expr f) a
substExpr x needle = hccata @Typeable (substExprF x needle)

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
fvF (ELaplaceF _ w) = K . unK $ w
fvF (EReturnF a) = K . unK $ a
fvF (EBindF a k) = K $ do
  a' <- unK a
  k' <- unK k
  return (S.union a' k')

fv :: FreshM m => (forall f. Expr f a) -> m (S.Set String)
fv = unK . hcata fvF

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. Expr f (Int -> Int)
example1 = toDeepRepr $ \(x :: Expr f Int) -> x

example2 :: Expr f Bool
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
  ExprF (K (m Doc)) a
  -> K (m Doc) a
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

prettyExpr :: FreshM m =>
  (forall f. Expr f a)
  -> m Doc
prettyExpr = unK . hcata prettyExprF
