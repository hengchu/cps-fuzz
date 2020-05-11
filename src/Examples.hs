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
      $(named "s123") <- xpar (xpar (lap 1.0 filter_sum) (lap 2.0 filter_sum)) (lap 3.0 filter_sum)
      let s1 = xpfst $ xpfst s123
      let s2 = xpsnd $ xpfst s123
      let s3 = xpsnd s123
      return (s1 + s2 + s3)

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
    minus_1 :: Name "loop_acc" (CPSFuzz f Number) -> CPSFuzz f (Distr Number)
    minus_1 (N v) = return $ v - 1

bag_filter_sum_noise6 :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_filter_sum_noise6 db =
  bag_filter_sum db $
    \filter_sum -> do
      $(named "s1") <- lap 1.0 filter_sum
      $(named "s2") <- lap 1.0 filter_sum
      bmap (plus (s1 * s2)) db $ \(N db2 :: Name "db_plus_1" _) ->
        bsum 2 db2 $ \(N filter_sum2 :: Name "filter_sum2" _) -> do
          $(named "s3") <- lap 2.0 filter_sum2
          return $ s1 + s3
  where
    plus :: CPSFuzz f Number -> Name "row" (CPSFuzz f Number) -> CPSFuzz f Number
    plus a (N b) = a + b

bag_partition_sum_noise :: forall f. CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Number)
bag_partition_sum_noise db =
  bpartition 3 part_fun db $ \(N [part1, part2, part3] :: Name "parts" _) ->
    bmapNothing 0 part1 $ \(N part1 :: Name "part1" _) ->
      bmapNothing 0 part2 $ \(N part2 :: Name "part2" _) ->
        bmapNothing 0 part3 $ \(N part3 :: Name "part3" _) ->
          bsum 2 part1 $ \(N s1 :: Name "sum_part1" _) ->
            bsum 3 part2 $ \(N s2 :: Name "sum_part2" _) ->
              bsum 4 part3 $ \(N s3 :: Name "sum_part3" _) ->
                lap 1.0 (s1 + s2 + s3)
  where
    part_fun :: Name "row" (CPSFuzz f Number) -> CPSFuzz f Int
    part_fun (N v) =
      if_
        (v %< -1)
        0
        ( if_
            (v %> 1)
            2
            1
        )

type Point = (Number, Number)
type Centroid3 = ((Point, Point), Point)

sqdist :: CPSFuzz f Point -> CPSFuzz f Point -> CPSFuzz f Number
sqdist p1 p2 =
  let p1x = xpfst p1
      p1y = xpsnd p1
      p2x = xpfst p2
      p2y = xpsnd p2
      xDiff = p1x - p2x
      yDiff = p1y - p2y
  in xDiff * xDiff + yDiff * yDiff

assign :: CPSFuzz f Point
       -> CPSFuzz f Point
       -> CPSFuzz f Point
       -> Name "point" (CPSFuzz f Point)
       -> CPSFuzz f Int
assign c1 c2 c3 (N p) =
  let d1 = sqdist c1 p
      d2 = sqdist c2 p
      d3 = sqdist c3 p
  in if_ (d1 %< d2 %&& d1 %< d3)
         0
         (if_ (d2 %< d1 %&& d2 %< d3)
              1
              2)

totalCoords :: Typeable r => CPSFuzz f (Bag Point) -> (CPSFuzz f Point -> CPSFuzz f r) -> CPSFuzz f r
totalCoords points k =
  bmap (projx @"point") points $ \(N xcoords :: Name "xcoords" _) ->
  bmap (projy @"point") points $ \(N ycoords :: Name "ycoords" _) ->
  bsum 1.0 xcoords $ \(N xsum :: Name "xsum" _) ->
  bsum 1.0 ycoords $ \(N ysum :: Name "ysum" _) ->
  k (xppair xsum ysum)

countPoints :: Typeable r => CPSFuzz f (Bag Point) -> (CPSFuzz f Number -> CPSFuzz f r) -> CPSFuzz f r
countPoints points k =
  bmap just1 points $ \(N ones :: Name "ones" _) ->
  bsum 1.0 ones $ \(N count :: Name "count" _) ->
  k count
  where
    just1 :: Name "point" (CPSFuzz f Point) -> CPSFuzz f Number
    just1 (N _) = 1

projx :: KnownSymbol s => Name s (CPSFuzz f Point) -> CPSFuzz f Number
projx (N v) = xpfst v

projy :: KnownSymbol s => Name s (CPSFuzz f Point) -> CPSFuzz f Number
projy (N v) = xpsnd v

initialCentroids :: CPSFuzz f Centroid3
initialCentroids = xppair (xppair c1 c2) c3
  where c1 = xppair 1.0 1.0
        c2 = xppair 0.5 0.5
        c3 = xppair 0.0 0.0

kmeans_iter :: CPSFuzz f Centroid3 -> CPSFuzz f (Bag Point) -> CPSFuzz f (Distr Centroid3)
kmeans_iter centroids db =
  let c3 = xpsnd centroids
      c2 = xpsnd (xpfst centroids)
      c1 = xpfst (xpfst centroids)
  in bpartition 3 (assign c1 c2 c3) db $ \(N [part1, part2, part3] :: Name "assigned_points" _) ->
     bmapNothing zeroPoint part1 $ \(N part1 :: Name "part1" _) ->
     bmapNothing zeroPoint part2 $ \(N part2 :: Name "part2" _) ->
     bmapNothing zeroPoint part3 $ \(N part3 :: Name "part3" _) ->
     totalCoords part1 $ \part1_total_coords ->
     countPoints part1 $ \part1_size ->
     totalCoords part2 $ \part2_total_coords ->
     countPoints part2 $ \part2_size ->
     totalCoords part3 $ \part3_total_coords ->
     countPoints part3 $ \part3_size ->
     let part1_total_coords_named = N part1_total_coords :: Name "part1_total_coords" _
         part2_total_coords_named = N part2_total_coords :: Name "part2_total_coords" _
         part3_total_coords_named = N part3_total_coords :: Name "part3_total_coords" _
         part1x = projx part1_total_coords_named
         part1y = projy part1_total_coords_named
         part2x = projx part2_total_coords_named
         part2y = projy part2_total_coords_named
         part3x = projx part3_total_coords_named
         part3y = projy part3_total_coords_named
     in do
       $(named "noised_values") <- xpar (lap 1.0 part1x)
                                        (xpar (lap 1.0 part1y)
                                              (xpar (lap 1.0 part1_size)
                                                    (xpar (lap 1.0 part2x)
                                                          (xpar (lap 1.0 part2y)
                                                                (xpar (lap 1.0 part2_size)
                                                                      (xpar (lap 1.0 part3x)
                                                                            (xpar (lap 1.0 part3y)
                                                                                  (lap 1.0 part3_size)
                                                                            )
                                                                      )
                                                                )
                                                          )
                                                    )
                                              )
                                        )
       let p1x = xpfst noised_values
           p1y = xpfst (xpsnd noised_values)
           p1len = xpfst (xpsnd (xpsnd noised_values))
           p2x = xpfst (xpsnd (xpsnd (xpsnd noised_values)))
           p2y = xpfst (xpsnd (xpsnd (xpsnd (xpsnd noised_values))))
           p2len = xpfst (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd noised_values)))))
           p3x = xpfst (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd noised_values))))))
           p3y = xpfst (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd noised_values)))))))
           p3len = xpsnd (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd (xpsnd noised_values)))))))
           c1 = xppair (p1x / p1len) (p1y / p1len)
           c2 = xppair (p2x / p2len) (p2y / p2len)
           c3 = xppair (p3x / p3len) (p3y / p3len)
           cs = xppair (xppair c1 c2) c3
       return cs
  where zeroPoint = xppair 0 0

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
