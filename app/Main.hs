module Main where

import Examples
import Extraction
import ExtractionPretty

main :: IO ()
main = do
  let Right py = compileAndExtract "db" bag_filter_sum_noise
  writeFile "extracted/bag_filter_sum_noise.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (kmeans_iter initialCentroids)
  writeFile "extracted/kmeans_iter.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (logistic_iter [0.0, 0.0, 0.0, 0.0, 0.0] 0.001)
  writeFile "extracted/logistic_iter.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (naive_bayes 10)
  writeFile "extracted/naive_bayes.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (pca 10 3)
  writeFile "extracted/pca.py" (pExtractionStr py)

  let Right py = compileAndExtract "db" (perceptron_iter [0.0, 0.0, 0.0, 0.0, 0.0])
  writeFile "extracted/perceptron_iter.py" (pExtractionStr py)
