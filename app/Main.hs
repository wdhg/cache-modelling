module Main where

import Control.Monad.ST
import Data.Foldable (foldr')
import Simulation

main :: IO ()
main = mapM_ print $ simulate 1 1024 65536 1000
