module Main where

import CacheType
import Control.Monad.ST
import Data.Foldable (foldr')
import Params
import Results
import Simulation
import System.Directory
import Text.JSON

experiments :: [Params]
experiments =
  [ Params
      { cacheType = FIFO,
        seed = 0,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 10
      },
    Params
      { cacheType = FIFO,
        seed = 1,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 100
      },
    Params
      { cacheType = FIFO,
        seed = 2,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 1000
      },
    Params
      { cacheType = FIFO,
        seed = 3,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 10000
      },
    Params
      { cacheType = FIFO,
        seed = 4,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 100000
      },
    Params
      { cacheType = FIFO,
        seed = 5,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 1000000
      },
    Params
      { cacheType = LRU,
        seed = 0,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 10
      },
    Params
      { cacheType = LRU,
        seed = 1,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 100
      },
    Params
      { cacheType = LRU,
        seed = 2,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 1000
      },
    Params
      { cacheType = LRU,
        seed = 3,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 10000
      },
    Params
      { cacheType = LRU,
        seed = 4,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 100000
      },
    Params
      { cacheType = LRU,
        seed = 5,
        cacheSize = 1024,
        itemCount = 65536,
        duration = 1000000
      }
  ]

runExperiment :: Params -> IO ()
runExperiment params = do
  let title = show (cacheType params) ++ "-" ++ show (duration params) ++ "-" ++ show (seed params)
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
main = mapM_ runExperiment experiments
