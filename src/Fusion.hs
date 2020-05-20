module Fusion where

import Syntax
import Type.Reflection

class UniformTuple a tup where
  flatten :: CPSFuzz f tup -> [CPSFuzz f a]

instance UniformTuple a a where
  flatten x = [x]

instance (Typeable a, Typeable tup, UniformTuple a tup) => UniformTuple a (a, tup) where
  flatten term = (xpfst term):(Fusion.flatten (xpsnd term))

data FuseVec :: (* -> *) -> * -> * where
  One :: Typeable a => CPSFuzz f (Distr a) -> FuseVec f a
  Par :: (Typeable a, Typeable tup, UniformTuple a tup) =>
    CPSFuzz f (Distr a) -> FuseVec f tup -> FuseVec f (a, tup)

fuseVec' ::
  Typeable a =>
  [CPSFuzz f (Distr a)] ->
  (forall tup. (Typeable tup, UniformTuple a tup) => FuseVec f tup -> r) -> r
fuseVec' []     _ = error "fuseVec': expected non empty list"
fuseVec' [x]    k = k $ One x
fuseVec' (x:xs) k =
  fuseVec' xs $ \tup -> k (Par x tup)

xparAll :: FuseVec f tup -> CPSFuzz f (Distr tup)
xparAll (One x)   = x
xparAll (Par x y) = xpar x (xparAll y)

fuseVec ::
  Typeable a =>
  [CPSFuzz f (Distr a)] -> CPSFuzz f (Distr (Vec a))
fuseVec xs =
  fuseVec' xs $ \fusedTup ->
  (xparAll fusedTup) >>=. \(N tup :: Name "fused_vector_tup" _) -> ret . xvlit . Fusion.flatten $ tup
