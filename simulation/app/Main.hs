module Main where

import CacheType
import Control.Monad (liftM5)
import Data.Foldable (foldr')
import Params
import Results
import Simulation
import System.Directory (createDirectoryIfMissing)
import Text.JSON (encode)

makeExperiments :: [CacheType] -> [Int] -> [Int] -> [Int] -> [Float] -> [Params]
makeExperiments = liftM5 Params

runExperiment :: Params -> IO ()
runExperiment params = do
  let title =
        show (cacheType params)
          ++ "-"
          ++ show (seed params)
          ++ "-"
          ++ show (cacheSize params)
          ++ "-"
          ++ show (itemCount params)
          ++ "-"
          ++ show (floor $ duration params)
  putStrLn $ "==== " ++ title ++ " ===="
  putStrLn "Running experiment..."
  let results = simulate params
      outputDir = "../simulation-output/"
      filename = outputDir ++ title ++ ".json"
  putStrLn "Done."
  putStrLn $ "Writing to file " ++ filename ++ "..."
  createDirectoryIfMissing True outputDir
  writeFile filename $ encode results
  putStrLn "Done."

main :: IO ()
main = mapM_ runExperiment $ makeExperiments [FIFO, LRU] [0 .. 4] [1024] [65536] [10, 100, 1000, 10000, 100000, 1000000]
