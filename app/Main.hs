{-# LANGUAGE RankNTypes #-}

module Main where

import Syntax
import Examples
import Extraction
import ExtractionPretty

logWriteFile :: String -> String -> IO ()
logWriteFile filePath prog = do
  putStrLn $ "writing to: " ++ filePath
  writeFile filePath prog

f1 :: IO ()
f1 = do
  let Right py = compileAndExtract "db" bag_filter_sum_noise
  logWriteFile "extracted/bag_filter_sum_noise.py" (pExtractionStr py)

f2 :: IO ()
f2 = do
  let Right py = compileAndExtract "db" (kmeans_iter initialCentroids)
  logWriteFile "extracted/kmeans_iter.py" (pExtractionStr py)

f3 :: IO ()
f3 = do
  let Right py = compileAndExtract "db" (logistic_iter [0.0, 0.0, 0.0, 0.0, 0.0] 0.001)
  logWriteFile "extracted/logistic_iter.py" (pExtractionStr py)

f4 :: IO ()
f4 = do
  let Right py = compileAndExtract "db" (naive_bayes 10)
  logWriteFile "extracted/naive_bayes.py" (pExtractionStr py)

f5 :: IO ()
f5 = do
  let Right py = compileAndExtract "db" (pca 10 3)
  logWriteFile "extracted/pca.py" (pExtractionStr py)

f6 :: IO ()
f6 = do
  let Right py = compileAndExtract "db" (perceptron_iter [0.0, 0.0, 0.0, 0.0, 0.0])
  logWriteFile "extracted/perceptron_iter.py" (pExtractionStr py)

f7 :: IO ()
f7 = do
  let Right py = compileAndExtract "db" simple_expm
  logWriteFile "extracted/simple_expm.py" (pExtractionStr py)

f8 :: IO ()
f8 = do
  let Right py = compileAndExtract "db" (aboveThreshold 10.0 1.0)
  logWriteFile "extracted/above_threshold.py" (pExtractionStr py)

f9 :: IO ()
f9 = do
  let Right py = compileAndExtract "db" (histogram 10 0 1)
  logWriteFile "extracted/histogram.py" (pExtractionStr py)

f10 :: IO ()
f10 = do
  let Right py = compileAndExtract "db" (vec_sum 10)
  logWriteFile "extracted/vec_sum.py" (pExtractionStr py)

f11 :: IO ()
f11 = do
  let Right py = compileAndExtract "db" (count_mean_sketch default_hash_funs)
  logWriteFile "extracted/count_mean_sketch.py" (pExtractionStr py)

f12 :: IO ()
f12 = do
  let Right py = compileAndExtract "db" (id3_iter [2, 3, 4, 5, 6, 7])
  logWriteFile "extracted/id3_iter.py" (pExtractionStr py)

f13 :: IO ()
f13 = do
  let Right py = compileAndExtract "db" (cdf 4 10)
  logWriteFile "extracted/cdf.py" (pExtractionStr py)

f14 :: IO ()
f14 = do
  let Right py = compileAndExtract "db" (range_query 4 10)
  logWriteFile "extracted/range_query.py" (pExtractionStr py)

f15 :: IO ()
f15 = do
  let
    fPoints :: forall f. [CPSFuzz f (Number, Number)]
    fPoints = [xppair 0.1 0.2]

    vPoints1 :: forall f. [CPSFuzz f Number]
    vPoints1 = map lit $ take 2 $ iterate (+0.1) 0

    vPoints :: forall f. [CPSFuzz f (Number, Number)]
    vPoints = [xppair x y | x <- vPoints1, y <- vPoints1]

    Right py = compileAndExtract "db" (kmedian_iter fPoints vPoints)
  logWriteFile "extracted/kmedian_iter_small.py" (pExtractionStr py)

f16 :: IO ()
f16 = do
  let
    fPoints :: forall f. [CPSFuzz f (Number, Number)]
    fPoints = [xppair 0.1 0.2, xppair 0.3 0.1]

    vPoints1 :: forall f. [CPSFuzz f Number]
    vPoints1 = map lit $ take 3 $ iterate (+0.1) 0

    vPoints :: forall f. [CPSFuzz f (Number, Number)]
    vPoints = [xppair x y | x <- vPoints1, y <- vPoints1]

    Right py = compileAndExtract "db" (kmedian_iter fPoints vPoints)
  logWriteFile "extracted/kmedian_iter_medium.py" (pExtractionStr py)

f17 :: IO ()
f17 = do
  let
    fPoints :: forall f. [CPSFuzz f (Number, Number)]
    fPoints = [xppair 0.1 0.2, xppair 0.5 0.3, xppair 0.3 0.1]

    vPoints1 :: forall f. [CPSFuzz f Number]
    vPoints1 = map lit $ take 5 $ iterate (+0.1) 0

    vPoints :: forall f. [CPSFuzz f (Number, Number)]
    vPoints = [xppair x y | x <- vPoints1, y <- vPoints1]

    Right py = compileAndExtract "db" (kmedian_iter fPoints vPoints)
  logWriteFile "extracted/kmedian_iter_large.py" (pExtractionStr py)


main :: IO ()
main = f1 >> f2 >> f3 >> f4 >> f5 >> f6 >> f7 >> f8 >> f9 >> f10 >> f11 >> f12 >> f13 >> f14 >> f15 >> f16 >> f17
