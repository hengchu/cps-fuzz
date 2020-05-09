{-# LANGUAGE RebindableSyntax #-}

module Examples where

import Data.String
import GHC.TypeLits
import HFunctor
import Names
import Syntax
import Type.Reflection
import Prelude hiding ((>>=), return)

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. CPSFuzz f (Int -> Int)
example1 = toDeepRepr $ \(N x :: Name "abc" (CPSFuzz f Int)) -> x

example2 :: forall f. CPSFuzz f Bool
example2 = xwrap . hinject' $ (XEVarF (UniqueName "y" 0))

-- Note that this "y" does not capture the "y" inside. It will get automatically
-- alpha-converted.
example3 :: forall f. CPSFuzz f (Int -> Bool)
example3 = toDeepRepr $ \(N _ :: (Name "foo" (CPSFuzz f Int))) -> (example2 @f)

example4 :: forall f. CPSFuzz f (Number -> Distr Number)
example4 = toDeepRepr $ \(N w :: Name "w" (CPSFuzz f Number)) -> do
  ($(named "noised")) <- lap 1.0 w
  ($(named "noised2")) <- lap 2.0 w
  return $ noised + noised2

example5 :: forall f. CPSFuzz f (Bag Number -> Number)
example5 = toDeepRepr @(CPSFuzz f) (\(N db :: Name "db" (CPSFuzz f _)) -> bag_filter_sum db id)

example6 :: forall f. CPSFuzz f (Bag Number -> Number)
example6 = toDeepRepr @(CPSFuzz f) (\(N db :: Name "db" (CPSFuzz f _)) -> needs_flatten db)

example7 :: forall f. CPSFuzz f Bool
example7 = app example3 1

bag_filter_sum ::
  Typeable r =>
  CPSFuzz f (Bag Number) ->
  (CPSFuzz f Number -> CPSFuzz f r) ->
  CPSFuzz f r
bag_filter_sum db k =
  bfilter gt_10 db $
    \($(named "gt_10_db")) -> bmapNothing 0 gt_10_db $
      \($(named "gt_10_db")) -> bsum 20 gt_10_db $
        \($(named "gt_10_sum")) -> bfilter lt_5 db $
          \($(named "lt_5_db")) -> bmapNothing 0 lt_5_db $
            \($(named "lt_5_db")) -> bsum 5 lt_5_db $
              \($(named "lt_5_sum")) -> k $ lt_5_sum + gt_10_sum
  where
    gt_10 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    gt_10 (N v) = v %> 10
    lt_5 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    lt_5 (N v) = v %< 5

needs_flatten :: CPSFuzz f (Bag Number) -> CPSFuzz f Number
needs_flatten db =
  bag_filter_sum (bmap plus_1 db $ \($(named "r")) -> r) id
  where
    plus_1 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Number
    plus_1 (N v) = v + 1

bag_filter_sum_noise :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise db =
  bag_filter_sum db $
    \filter_sum -> do
      $(named "s1'") <- lap 1.0 filter_sum
      $(named "s2'") <- lap 2.0 filter_sum
      return (s1' + s2')

bag_filter_sum_noise2 :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise2 db =
  bfilter gt_10 db $
    \($(named "gt_10_db")) -> bmapNothing 0 gt_10_db $
      \($(named "gt_10_db")) -> bsum 20 gt_10_db $
        \($(named "gt_10_sum")) -> bfilter lt_5 db $
          \($(named "lt_5_db")) -> bmapNothing 0 lt_5_db $
            \($(named "lt_5_db")) -> bsum 5 lt_5_db $
              \($(named "lt_5_sum")) -> do
                let filter_sum = lt_5_sum + gt_10_sum
                $(named "s1'") <- lap 1.0 filter_sum
                $(named "s2'") <- lap 2.0 s1'
                return (s1' + s2')
  where
    gt_10 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    gt_10 (N v) = v %> 10
    lt_5 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    lt_5 (N v) = v %< 5

bag_filter_sum_noise3 :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise3 db =
  bfilter gt_10 db $
    \($(named "gt_10_db")) -> bmapNothing 0 gt_10_db $
      \($(named "gt_10_db")) -> bsum 20 gt_10_db $
        \($(named "gt_10_sum")) -> do
          $(named "s1'") <- lap 1.0 gt_10_sum
          bfilter lt_5 db $
            \($(named "lt_5_db")) -> bmapNothing 0 lt_5_db $
              \($(named "lt_5_db")) -> bsum 5 lt_5_db $
                \($(named "lt_5_sum")) -> do
                  $(named "s2'") <- lap 2.0 lt_5_sum
                  return (s1' + s2')
  where
    gt_10 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    gt_10 (N v) = v %> 10
    lt_5 :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Bool
    lt_5 (N v) = v %< 5

bag_filter_sum_noise4 :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise4 db =
  bag_filter_sum db $
    \filter_sum -> do
      $(named "s1'") <- lap 1.0 filter_sum
      return (s1' + filter_sum)

bag_filter_sum_noise5 :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise5 db =
  bag_filter_sum db $
    \filter_sum -> do
      $(named "s1'") <- lap 1.0 filter_sum
      $(named "r'") <- loop s1' gt_5 minus_1 --(resample filter_sum)
      return r'
  where
    gt_5 :: Name "loop_acc" (CPSFuzz f Number) -> CPSFuzz f Bool
    gt_5 (N v) = v %> 5

    resample ::
      CPSFuzz f Number ->
      Name "loop_acc" (CPSFuzz f Number) ->
      CPSFuzz f (Distr Number)
    resample filter_sum _ = lap 1.0 filter_sum

    minus_1 ::  Name "loop_acc" (CPSFuzz f Number) -> CPSFuzz f (Distr Number)
    minus_1 (N v) = return $ v - 1

-- #######################
-- # FUNNY SYNTAX TRICKS #
-- #######################
(>>=) ::
  (SynMonad h m, KnownSymbol s, Typeable a, Typeable b) =>
  h (m a) ->
  (Name s (h a) -> h (m b)) ->
  h (m b)
(>>=) = (>>=.)

return :: (SynMonad h m, Typeable a) => h a -> h (m a)
return = ret
