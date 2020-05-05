{-# LANGUAGE RebindableSyntax #-}

module Examples where

import Data.String
import Prelude hiding ((>>=), return)
import Syntax
import HFunctor
import GHC.TypeLits
import Type.Reflection

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. CPSFuzz f (Int -> Int)
example1 = toDeepRepr $ \(N x :: Name "abc" (CPSFuzz f Int)) -> x

example2 :: forall f. CPSFuzz f Bool
example2 = xwrap . hinject' $ (XEVarF "y")

-- Note that this "y" does not capture the "y" inside. It will get automatically
-- alpha-converted.
example3 :: forall f. CPSFuzz f (Int -> Bool)
example3 = toDeepRepr $ \(N _ :: (Name "foo" (CPSFuzz f Int))) -> (example2 @f)

example4 :: forall f. CPSFuzz f (Number -> Distr Number)
example4 = toDeepRepr $ \(N w :: Name "w" (CPSFuzz f Number)) -> do
  ($(named "noised")) <- lap 1.0 w
  ($(named "noised2")) <- lap 2.0 w
  return $ noised + noised2

bag_filter_sum :: CPSFuzz f (Bag Number) -> CPSFuzz f Number
bag_filter_sum db =
  bfilter gt_10 db $
  \($(named "gt_10_db"))  -> bmapNothing 0 gt_10_db $
  \($(named "gt_10_db"))  -> bsum 20 gt_10_db $
  \($(named "gt_10_sum")) -> bfilter lt_5 db $
  \($(named "lt_5_db"))   -> bmapNothing 0 lt_5_db $
  \($(named "lt_5_db"))   -> bsum 5 lt_5_db $
  \($(named "lt_5_sum"))  -> lt_5_sum + gt_10_sum
  where
    gt_10 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    gt_10 (N v) = v %> 10

    lt_5 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    lt_5 (N v) = v %< 5

bag_filter_sum_noise :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise db =
  share (bag_filter_sum db) . toDeepRepr $
  \(N filter_sum :: Name "filter_sum" (CPSFuzz f Number)) -> do
  $(named "s1'") <- lap 1.0 filter_sum
  $(named "s2'") <- lap 2.0 filter_sum
  return (s1' + s2')

-- #######################
-- # FUNNY SYNTAX TRICKS #
-- #######################
(>>=) :: (SynMonad h m, KnownSymbol s, Typeable a, Typeable b)
    => h (m a) -> (Name s (h a) -> h (m b)) -> h (m b)
(>>=) = (>>=.)

return :: (SynMonad h m, Typeable a) => h a -> h (m a)
return = ret
