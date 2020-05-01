{-# LANGUAGE AllowAmbiguousTypes #-}

-- | An experimental module that tries to re-implement the syntax in terms
-- of fixpoint of GADTs. Mostly useless.
module Syntax where

--import Lib
--import Names

import Data.Kind
import Type.Reflection

newtype K a b = K a
  deriving (Show, Eq, Ord, Functor)

instance Semigroup m => Semigroup (K m b) where
  (K a) <> (K b) = K (a <> b)

instance Monoid m => Monoid (K m b) where
  mempty = K mempty

-- | Take the fixpoint of a functor-functor.
data HFix (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
  HFix  :: h (HFix h f) a -> HFix h f a
  Place :: f a -> HFix h f a

class Subst (f :: * -> *) where
  abstract :: f b -> Prim a -> Prim a -> f b

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

data ExprF :: (* -> *) -> * -> * where
  EVarF :: Typeable a => String -> ExprF r a
  -- Notes: we could make the HOAS representation of type (Prim a) -> (r b),
  -- thus, making sure the lambda is parametric in terms of input, and get rid
  -- of r in the negative position.
  --
  -- This can potentially allow us to get rid of `Place`, and make `ExprF` a
  -- functor instead of an exponential functor.
  --
  -- To map over a lambda, we generate a fresh `Prim`, run the HOAS lambda over
  -- the fresh `Prim`, which gives us the body of the lambda with type `r
  -- b`. The map function has type `forall a. r a -> g a`. So, we can get an `g
  -- b` out of this. However, we need a function `Prim a -> g b`. So, we need to
  -- re-abstract out the `Prim a` parameter that we just fed into the lambda. Is
  -- that possible???
  --
  -- Basically, we need to write a function with type
  -- abstract :: g b -> Prim a -> Prim a -> g b
  --             ^ the body of the lambda with the fresh prim we just fed into it
  --                    ^ the fresh prim that we used
  --                              ^ the new abstract prim
  --
  -- This seems like it's basically substitution. But, we don't know anything
  -- about `g`. So, we need `g` to be some a functor-like structure that
  -- supports substitution.
  ELamF :: (Typeable a, Typeable b) => (r a -> r b) -> ExprF r (a -> b)
  EAppF :: (Typeable a, Typeable b) => r (a -> b) -> r a -> ExprF r b

data Prim :: * -> * where
  Prim :: String -> Prim a
  deriving (Show, Eq, Ord)

instance HXFunctor ExprF where
  hxmap f g =
    \case
      EVarF x -> EVarF x
      ELamF lam -> ELamF $ (f . lam . g)
      EAppF lam arg -> EAppF (f lam) (f arg)

instance HXCFunctor Typeable ExprF where
  hxcmap f g =
    \case
      EVarF x -> EVarF x
      ELamF lam -> ELamF (f . lam . g)
      EAppF lam arg -> EAppF (f lam) (f arg)

-- | The functor-like variable `f` is the interpretation domain. Examples
-- include: `Doc` for pretty-printing, `Identity` for evaluation, etc.
type Expr (f :: * -> *) = HFix ExprF f

inject :: h (HFix h f) a -> HFix h f a
inject = HFix

place :: f a -> HFix h f a
place = Place

injectExprF :: ExprF (HFix ExprF f) a -> Expr f a
injectExprF = inject

lam :: (Typeable a, Typeable b) => (Expr f a -> Expr f b) -> Expr f (a -> b)
lam t = inject (ELamF t)

app :: (Typeable a, Typeable b) => Expr f (a -> b) -> Expr f a -> Expr f b
app f t = inject (EAppF f t)

-- | Catamorphism over a functor-functor.
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
      Nothing -> inject term
    else inject term
substExprF _ _ term = inject term

substExpr :: (Typeable r, Typeable a) => String -> (Expr f) r -> (Expr (Expr f)) a -> (Expr f) a
substExpr x needle = hccata @Typeable (substExprF x needle)

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. Expr f (Int -> Int)
example1 = toDeepRepr $ \(x :: Expr f Int) -> x

example2 :: Expr f Bool
example2 = inject (EVarF "x")

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
