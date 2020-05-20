module Main where

import Syntax
import Examples
import Extraction
import ExtractionPretty

logWriteFile :: String -> String -> IO ()
logWriteFile filePath prog = do
  putStrLn $ "writing to: " ++ filePath
  writeFile filePath prog

main :: IO ()
main = do
  let Right py = compileAndExtract "db" bag_filter_sum_noise
  logWriteFile "extracted/bag_filter_sum_noise.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (kmeans_iter initialCentroids)
  logWriteFile "extracted/kmeans_iter.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (logistic_iter [0.0, 0.0, 0.0, 0.0, 0.0] 0.001)
  logWriteFile "extracted/logistic_iter.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (naive_bayes 10)
  logWriteFile "extracted/naive_bayes.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (pca 10 3)
  logWriteFile "extracted/pca.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (perceptron_iter [0.0, 0.0, 0.0, 0.0, 0.0])
  logWriteFile "extracted/perceptron_iter.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" simple_expm
  logWriteFile "extracted/simple_expm.py" (pExtractionStr py)

  let
    fPoints :: forall f. [CPSFuzz f (Number, Number)]
    fPoints = [xppair 0.1 0.2, xppair 0.5 0.3, xppair 0.3 0.1]

    vPoints1 :: forall f. [CPSFuzz f Number]
    vPoints1 = map lit $ take 5 $ iterate (+0.1) 0

    vPoints :: forall f. [CPSFuzz f (Number, Number)]
    vPoints = [xppair x y | x <- vPoints1, y <- vPoints1]

    Right py = compileAndExtract "db" (kmedian_iter fPoints vPoints)
  logWriteFile "extracted/kmedian_iter.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (aboveThreshold 10.0 1.0)
  logWriteFile "extracted/above_threshold.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (histogram 10 0 1)
  logWriteFile "extracted/histogram.py" (pExtractionStr py)
