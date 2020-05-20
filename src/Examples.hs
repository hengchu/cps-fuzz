{-# LANGUAGE RebindableSyntax #-}

module Examples where

import Data.String
import GHC.TypeLits
import HFunctor
import Names
import Syntax hiding (add)
import Type.Reflection
import Fusion
import Prelude hiding ((>>=), return, exp, log)

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

kmeans_iter_k ::
  Typeable r =>
  CPSFuzz f Centroid3 ->
  CPSFuzz f (Bag Point) ->
  (CPSFuzz f Centroid3 -> CPSFuzz f (Distr r)) ->
  CPSFuzz f (Distr r)
kmeans_iter_k centroids db kont =
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
     countPoints part3 $ \part3_size -> do
       $(named "cs") <- noise_and_release
                          part1_total_coords part1_size
                          part2_total_coords part2_size
                          part3_total_coords part3_size
       kont cs
  where zeroPoint = xppair 0 0

        noise_and_release part1_total_coords part1_size part2_total_coords part2_size part3_total_coords part3_size = do
          let part1_total_coords_named = N part1_total_coords :: Name "part1_total_coords" _
              part2_total_coords_named = N part2_total_coords :: Name "part2_total_coords" _
              part3_total_coords_named = N part3_total_coords :: Name "part3_total_coords" _
              part1x = projx part1_total_coords_named
              part1y = projy part1_total_coords_named
              part2x = projx part2_total_coords_named
              part2y = projy part2_total_coords_named
              part3x = projx part3_total_coords_named
              part3y = projy part3_total_coords_named

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

kmeans_iter ::
  CPSFuzz f Centroid3 ->
  CPSFuzz f (Bag Point) ->
  CPSFuzz f (Distr Centroid3)
kmeans_iter cs db = kmeans_iter_k cs db return

kmeans ::
  CPSFuzz f (Bag Point) ->
  CPSFuzz f (Distr Centroid3)
kmeans db = do
  kmeans_iter_k initialCentroids db $ \cs1 -> do
    kmeans_iter_k cs1 db $ \cs2 -> do
      kmeans_iter cs2 db

type Weights = Vec Number
type Row = Vec Number

add :: forall f. CPSFuzz f Weights -> CPSFuzz f Weights -> CPSFuzz f Weights
add v1 v2 = loopPure initialAcc (cond $ xlength v1) iter
  where
    initialAcc :: CPSFuzz f Weights
    initialAcc = xvlit []

    cond :: CPSFuzz f Int -> Name "curr_acc" (CPSFuzz f Weights) -> CPSFuzz f Bool
    cond n (N v) = xlength v %< n

    iter :: Name "curr_acc" (CPSFuzz f Weights) -> CPSFuzz f Weights
    iter (N v) =
      let idx = xlength v
      in xconcat v (xvlit [v1 `xindex` idx + v2 `xindex` idx])

dot :: CPSFuzz f Weights -> CPSFuzz f Weights -> CPSFuzz f Number
dot v1 v2 = xpsnd (loopPure initialAcc (cond $ xlength v1) (iter v1 v2))
  where
    initialAcc :: CPSFuzz f (Int, Number)
    initialAcc = xppair 0 0

    cond :: CPSFuzz f Int -> Name "curr_acc" (CPSFuzz f (Int, Number)) -> CPSFuzz f Bool
    cond n (N acc) = xpfst acc %< n

    iter :: CPSFuzz f Weights -> CPSFuzz f Weights -> Name "curr_acc" (CPSFuzz f (Int, Number)) -> CPSFuzz f (Int, Number)
    iter v1 v2 (N acc) =
      let currIdx = xpfst acc
          currSum = xpsnd acc
      in xppair (currIdx+1) (currSum + xindex v1 currIdx * xindex v2 currIdx)

scale :: forall f. CPSFuzz f Number -> CPSFuzz f Weights -> CPSFuzz f Weights
scale k v = loopPure initialAcc (cond $ xlength v) iter
  where
    initialAcc :: CPSFuzz f Weights
    initialAcc = xvlit []

    cond :: CPSFuzz f Int -> Name "curr_acc" (CPSFuzz f Weights) -> CPSFuzz f Bool
    cond n (N acc) = xlength acc %< n

    iter :: Name "curr_acc" (CPSFuzz f Weights) -> CPSFuzz f Weights
    iter (N acc) =
      let idx = xlength acc
      in xconcat acc (xvlit [k * (v `xindex` idx)])

outer :: forall f. CPSFuzz f (Vec Number) -> CPSFuzz f (Vec Number) -> CPSFuzz f (Vec (Vec Number))
outer u v = loopPure initialAcc (cond $ xlength u) iter
  where
    initialAcc :: CPSFuzz f (Vec (Vec Number))
    initialAcc = xvlit []

    cond :: CPSFuzz f Int -> Name "curr_acc" (CPSFuzz f (Vec (Vec Number))) -> CPSFuzz f Bool
    cond n (N acc) = xlength acc %< n

    iter :: Name "curr_acc" (CPSFuzz f (Vec (Vec Number))) -> CPSFuzz f (Vec (Vec Number))
    iter (N acc) =
      let idx = xlength acc
          ui = u `xindex` idx
          ui_v = scale ui v
      in xconcat acc (xvlit [ui_v])

sequenceVec ::
  (Typeable a, Typeable r) =>
  [CPSFuzz f (Distr a)] ->
  (CPSFuzz f (Vec a) -> CPSFuzz f (Distr r)) ->
  CPSFuzz f (Distr r)
sequenceVec xs k = do
  $(named "fused_samples") <- fuseVec xs
  k fused_samples

logistic_iter_k_unsafe ::
  forall f r.
  Typeable r =>
  Int ->
  CPSFuzz f Weights ->
  CPSFuzz f Number ->
  CPSFuzz f Number ->
  CPSFuzz f (Bag Row) ->
  (CPSFuzz f Weights -> CPSFuzz f (Distr r)) ->
  CPSFuzz f (Distr r)
logistic_iter_k_unsafe dim weights learning_rate db_size db k =
  bmap gradient db $ \((N gradients) :: Name "gradients" _) -> do
  $(named "new_weights") <- descent gradients (dim-1) []
  k new_weights
  where
    gradient :: Name "row" (CPSFuzz f Weights) -> CPSFuzz f Weights
    gradient (N row) = scale (factor weights row) row

    -- clip and noise gradient at index j
    descent :: CPSFuzz f (Bag Weights) -> Int -> [CPSFuzz f (Distr Number)] -> CPSFuzz f (Distr Weights)
    descent gs j acc
      | j < 0 = sequenceVec acc $
        \noised_gradient -> return (add weights (scale (learning_rate / db_size) noised_gradient))
      | otherwise =
        bmap (\(N g :: Name "g" _) -> g `xindex` (lit j)) gs $ \(N gs_j :: Name "gs_j" _) ->
          bsum 1.0 gs_j $ \(N gs_j_sum :: Name "gs_j_sum" _) ->
          descent gs (j-1) ((lap 1.0 gs_j_sum):acc)

    factor :: CPSFuzz f Weights -> CPSFuzz f Weights -> CPSFuzz f Number
    factor w r =
      let p = dot w r
          n = xlength r
          y = r `xindex` (n - 1)
      in y * (1.0 - 1.0 / (1.0 + exp (-1.0 * y * p)))

logistic_iter_unsafe ::
  Int ->
  CPSFuzz f Weights ->
  CPSFuzz f Number ->
  CPSFuzz f Number ->
  CPSFuzz f (Bag Row) ->
  CPSFuzz f (Distr Weights)
logistic_iter_unsafe dim weights learning_rate db_size db =
  logistic_iter_k_unsafe dim weights learning_rate db_size db return

logistic_iter ::
  [CPSFuzz f Number] ->
  CPSFuzz f Number ->
  CPSFuzz f (Bag Row) ->
  CPSFuzz f (Distr Weights)
logistic_iter weights learning_rate db = do
  bmap (\(N _ :: Name "row" _) -> 1) db $ \(N ones :: Name "ones" _) ->
    bsum 1 ones $ \(N count :: Name "count" _) -> do
    $(named "db_size") <- lap 1.0 count
    logistic_iter_unsafe (length weights) (xvlit weights) learning_rate db_size db

naive_bayes ::
  forall f.
  Int ->
  CPSFuzz f (Bag Row) ->
  CPSFuzz f (Distr Weights)
naive_bayes row_size db =
  bmap get_label db $ \(N all_labels :: Name "all_labels" _) ->
  bsum 1.0 all_labels $ \(N label_sum :: Name "label_sum" _) -> do
  $(named "noised_label_sum") <- lap 1.0 label_sum
  update (row_size - 1) [] $ \noised_sum_pairs -> do
    let n = xlength noised_sum_pairs
        initialAcc = xvlit []
        cond :: Name "curr_acc" _ -> _
        cond (N acc) = xlength acc %< n
        iter :: Name "curr_acc" _ -> _
        iter (N acc) =
          let idx = xlength acc
              noised_pos_sum = xpfst (noised_sum_pairs `xindex` idx)
              noised_neg_sum = xpsnd (noised_sum_pairs `xindex` idx)
              theta_pos = noised_pos_sum / noised_label_sum
              theta_neg = noised_neg_sum / noised_label_sum
              v = log (theta_pos / theta_neg) - log((1-theta_pos) / (1-theta_neg))
          in acc `xconcat` (xvlit [v])
    return $ loopPure initialAcc cond iter
  where
    get_label :: Name "row" (CPSFuzz f Row) -> CPSFuzz f Number
    get_label (N row) = if_ (row `xindex` (lit $ row_size - 1) %== 1.0) 1.0 0.0

    update ::
      Typeable r =>
      Int ->
      [CPSFuzz f (Distr (Number, Number))] ->
      (CPSFuzz f (Vec (Number, Number)) -> CPSFuzz f (Distr r)) ->
      CPSFuzz f (Distr r)
    update j acc k
      | j < 0 = do
          $(named "noised_sum_pairs") <- sequenceVec acc return
          k noised_sum_pairs
      | otherwise =
        bmap (filter_pos_feature j) db $ \(N pos_feature_j :: Name "pos_feature_j" _) ->
          bsum 1.0 pos_feature_j $ \(N pos_feature_j_sum :: Name "pos_feature_j_sum" _) ->
          bmap (filter_neg_feature j) db $ \(N neg_feature_j :: Name "neg_feature_j" _) ->
          bsum 1.0 neg_feature_j $ \(N neg_feature_j_sum :: Name "neg_feature_j_sum" _) ->
          let this = xpar (lap 1.0 pos_feature_j_sum) (lap 1.0 neg_feature_j_sum)
          in update (j-1) (this:acc) k

    filter_pos_feature :: Int -> Name "row" (CPSFuzz f Row) -> CPSFuzz f Number
    filter_pos_feature j (N row) = if_ (row `xindex` (lit $ row_size-1) %== 1.0) (row `xindex` (lit j)) 0.0

    filter_neg_feature ::  Int -> Name "row" (CPSFuzz f Row) -> CPSFuzz f Number
    filter_neg_feature j (N row) = if_ (row `xindex` (lit $ row_size-1) %== 0.0) (row `xindex` (lit j)) 0.0

pca ::
  Int ->
  CPSFuzz f Int ->
  CPSFuzz f (Bag Row) ->
  CPSFuzz f (Distr (Vec Row))
pca row_size k db = do
  compress row_size db [] $ \compressed -> do
    let m = outer compressed compressed
    return $ xslice m 0 k
  where
    compress ::
      Typeable r =>
      Int ->
      CPSFuzz f (Bag Row) ->
      [CPSFuzz f (Distr Number)] ->
      (CPSFuzz f Row -> CPSFuzz f (Distr r)) ->
      CPSFuzz f (Distr r)
    compress j db acc k
      | j < 0 = do
          $(named "compressed") <- sequenceVec acc return
          k compressed
      | otherwise =
        bmap (get_coord j) db $ \(N db_j :: Name "db_j" _) ->
          bsum 1.0 db_j $ \(N db_j_sum :: Name "db_j_sum" _) ->
          compress (j-1) db ((lap 1.0 db_j_sum):acc) k

    get_coord :: Int -> Name "row" (CPSFuzz f Row) -> CPSFuzz f Number
    get_coord j (N row) = row `xindex` (lit j)

perceptron_iter_k_unsafe ::
  forall r f.
  Typeable r =>
  Int ->
  CPSFuzz f Weights ->
  CPSFuzz f (Bag Row) ->
  (CPSFuzz f Weights -> CPSFuzz f (Distr r)) ->
  CPSFuzz f (Distr r)
perceptron_iter_k_unsafe row_size weights db k =
  bmap is_correctly_classified db $ \(N incorrects :: Name "incorrectly_classified" _) ->
  bsum 1.0 incorrects $ \(N incorrect_count :: Name "incorrect_count" _) -> do
  $(named "noised_incorrect_count") <- lap 1.0 incorrect_count
  update (row_size - 2) noised_incorrect_count []
  where
    is_correctly_classified :: Name "row" (CPSFuzz f Row) -> CPSFuzz f Number
    is_correctly_classified (N row) =
      let y = row `xindex` (lit $ row_size - 1)
          x = xslice row 0 (lit $ row_size - 1)
          p = y * dot weights x
      in if_ (p %< 0) 1 0

    get_incorrectly_classified_coord :: Int -> Name "row" (CPSFuzz f Row) -> CPSFuzz f Number
    get_incorrectly_classified_coord j (N row) =
      let y = row `xindex` (lit $ row_size - 1)
          x = xslice row 0 (lit $ row_size - 1)
          p = y * dot weights x
      in if_ (p %< 0) (row `xindex` lit j) 0

    update ::
      Int ->
      CPSFuzz f Number ->
      [CPSFuzz f (Distr Number)] ->
      CPSFuzz f (Distr r)
    update j noised_incorrect_count acc
      | j < 0 = sequenceVec acc $ k . add weights . scale (1 / noised_incorrect_count)
      | otherwise =
        bmap (get_incorrectly_classified_coord j) db $ \(N db_j :: Name "db_j" _) ->
          bsum 1.0 db_j $ \(N db_j_sum :: Name "db_j_sum" _) ->
          update (j-1) noised_incorrect_count $ (lap 1.0 db_j_sum):acc

perceptron_iter_unsafe ::
  forall f.
  Int ->
  CPSFuzz f Weights ->
  CPSFuzz f (Bag Row) ->
  CPSFuzz f (Distr Weights)
perceptron_iter_unsafe row_size weights db =
  perceptron_iter_k_unsafe row_size weights db return

perceptron_iter ::
  forall f.
  [CPSFuzz f Number] ->
  CPSFuzz f (Bag Row) ->
  CPSFuzz f (Distr Weights)
perceptron_iter weights db =
  perceptron_iter_unsafe (length weights + 1) (xvlit weights) db

simple_expm :: CPSFuzz f (Bag Number) -> CPSFuzz f (Distr Int)
simple_expm _ =
  expm (xvlit [1, 2, 3])

kmedian_cost1 ::
  Typeable r =>
  CPSFuzz f Point ->
  CPSFuzz f (Bag Point) ->
  (CPSFuzz f Number -> CPSFuzz f r) ->
  CPSFuzz f r
kmedian_cost1 pt db k =
  bmap (compute_square_dist pt) db $ \(N distances :: Name "distances" _) ->
  bsum 1.0 distances $ \(N sum_distances :: Name "sum_distances" _) ->
  k sum_distances
  where
    compute_square_dist :: CPSFuzz f Point -> Name "row" (CPSFuzz f Point) -> CPSFuzz f Number
    compute_square_dist pt (N row) = sqdist pt row

kmedian_cost ::
  Typeable r =>
  [CPSFuzz f Point] ->
  CPSFuzz f (Bag Point) ->
  (CPSFuzz f Number -> CPSFuzz f r) ->
  CPSFuzz f r
kmedian_cost []     _  _ = error "kmedian_cost: expected non-empty list"
kmedian_cost [x]    db k = kmedian_cost1 x db k
kmedian_cost (x:xs) db k =
  kmedian_cost1 x db $ \dist_x ->
  kmedian_cost xs db $ \dist_xs ->
  k $ if_ (dist_x %< dist_xs) dist_x  dist_xs

-- Given set a, and set b, yield a - x + y where x is from a and y is from b
kmedian_cart_prod :: [a] -> [a] -> [([a], Int, Int)]
kmedian_cart_prod a b =
  let rangeA = [0..length a-1]
      rangeB = [0..length b-1]
  in concat $ for rangeA $ \idxA ->
       case splitAt idxA a of
         (left, _:right) -> for rangeB (\idxB -> (left ++ [b !! idxB] ++ right, idxA, idxB))
         _ -> []
    where for = flip map

kmedian_iter ::
  -- | median candidates F_i
  [CPSFuzz f Point] ->
  -- | total set of points V
  [CPSFuzz f Point] ->
  -- | Input database
  CPSFuzz f (Bag Point) ->
  -- | Returns a new set of candidates F_i+1
  CPSFuzz f (Distr (Vec Point))
kmedian_iter f v db =
  let candidates_xy = kmedian_cart_prod f v
      candidates = map (\(x, _, _) -> x) candidates_xy
      xy         = map (\(_, x, y) -> xppair (lit x) (lit y)) candidates_xy
  in go xy candidates []
  where go xy []     acc = do
          $(named "selected_pair") <- expm (xvlit acc)
          let xy_idx = (xvlit xy) `xindex` selected_pair
              x_idx = xpfst xy_idx
              y_idx = xpsnd xy_idx
              xs = xvlit f
              n = length f
              left_x = xslice xs 0 x_idx
              right_x = xslice xs (x_idx+1) (lit n)
              chosen = (xvlit v) `xindex` y_idx
          return $ left_x `xconcat` (xvlit [chosen]) `xconcat` right_x
        go xy (x:xs) acc =
          kmedian_cost x db $ \x_cost -> go xy xs (x_cost:acc)

aboveThreshold ::
  -- | guess
  CPSFuzz f Number ->
  -- | threshold
  CPSFuzz f Number ->
  -- | Input db
  CPSFuzz f (Bag Number) ->
  -- | Returns new result if above threshold
  CPSFuzz f (Distr Number)
aboveThreshold guess thresh db =
  bmap (\(N row :: Name "row" _) -> row) db $ \(N db :: Name "db" _) ->
  bsum 1.0 db $ \(N sum :: Name "sum" _) -> do
  $(named "new_result") <- lap 1.0 sum
  return $ if_ (new_result - guess %> thresh) (new_result) (guess)

histogram_bins ::
  Int ->
  CPSFuzz f Number ->
  CPSFuzz f Number ->
  [CPSFuzz f (Number, Number)]
histogram_bins n start end =
  reverse $ go n start ((end - start) / (lit . fromIntegral $ n)) []
  where
    go 0 _    _     acc = acc
    go n left width acc =
      go (n-1) (left+width) width ((xppair left (left+width)):acc)

histogram ::
  Int ->
  CPSFuzz f Number ->
  CPSFuzz f Number ->
  CPSFuzz f (Bag Number) ->
  CPSFuzz f (Distr (Vec Number))
histogram nbuckets start end db =
  let bins = histogram_bins nbuckets start end
  in go bins []
  where
    go []       acc = sequenceVec acc return
    go (bin:xs) acc =
      bmap (is_in bin) db $ \(N counts :: Name "bin_counts" _) ->
      bsum 1.0 counts $ \(N counts_sum :: Name "bin_counts_sum" _) ->
      go xs ((lap 1.0 counts_sum):acc)

    is_in :: CPSFuzz f (Number, Number) -> Name "row" (CPSFuzz f Number) -> CPSFuzz f Number
    is_in bin (N row) =
      let left = xpfst bin
          right = xpsnd bin
      in if_ (left %<= row %&& row %< right) 1 0

vec_sum ::
  Int ->
  CPSFuzz f (Bag (Vec Number)) ->
  CPSFuzz f (Distr (Vec Number))
vec_sum row_size db =
  go (row_size - 1) []
  where
    get_coord :: Int -> Name "row" (CPSFuzz f (Vec Number)) -> CPSFuzz f Number
    get_coord j (N row) = row `xindex` (lit j)

    go j acc
      | j < 0 = sequenceVec acc return
      | otherwise =
        bmap (get_coord j) db $ \(N coords_j :: Name "coords_j" _) ->
          bsum 1.0 coords_j $ \(N coords_j_sum :: Name "coords_j_sum" _) ->
          go (j-1) ((lap 1.0 coords_j_sum):acc)

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
