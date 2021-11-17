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
  let results = simulate params
      filename = show (cacheType params) ++ "-" ++ show (duration params) ++ "-" ++ show (seed params) ++ ".json"
  writeFile filename $ encode results

main :: IO ()
main = mapM_ runExperiment experiments
