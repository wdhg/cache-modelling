module Main where

import CacheType
import Control.Monad.ST
import Data.Foldable (foldr')
import Params
import Results
import Simulation
import Text.JSON

experiments :: [Params]
experiments =
  [ Params
      { cacheType = FIFO,
        seed = 0,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 1000
      }
  ]

runExperiment :: Params -> IO ()
runExperiment params = do
  let title = show (cacheType params) ++ "-" ++ show (duration params) ++ "-" ++ show (seed params)
  putStrLn $ "==== " ++ title ++ " ===="
  putStrLn "Running experiment..."
  let results = simulate params
      filename = title ++ ".json"
  putStrLn "Done."
  putStrLn $ "Writing to file " ++ filename ++ "..."
  writeFile filename $ encode results
  putStrLn "Done."

main :: IO ()
main = mapM_ runExperiment experiments
