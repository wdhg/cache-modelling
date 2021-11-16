{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simulation where

import Control.Monad.ST
import Data.Array.ST
import Data.STRef

data FIFOCache :: * -> * where
  FIFOCache ::
    { array :: STArray s Int Int,
      nextIndex :: STRef s Int
    } ->
    FIFOCache s

cachedIn :: Int -> FIFOCache s -> ST s Bool
cachedIn x cache = do
  xs <- getElems $ array cache
  return (x `elem` xs)

getNextIndex :: FIFOCache s -> ST s Int
getNextIndex cache = do
  index <- readSTRef $ nextIndex cache
  (lowerBound, upperBound) <- getBounds $ array cache
  if index < upperBound
    then writeSTRef (nextIndex cache) (succ index)
    else writeSTRef (nextIndex cache) lowerBound
  return index

-- x is not in cache
stash' :: Int -> FIFOCache s -> ST s ()
stash' x cache = do
  index <- getNextIndex cache
  writeArray (array cache) index x
  return ()

stash :: Int -> FIFOCache s -> ST s ()
stash x cache = do
  alreadyCached <- x `cachedIn` cache
  if alreadyCached
    then return ()
    else stash' x cache

simulateFIFO :: Int -> IO ()
simulateFIFO size = print $
  runST $ do
    initArray <- newArray (0, size - 1) (-1) :: ST s (STArray s Int Int)
    initNextIndex <- newSTRef 0
    let cache = FIFOCache initArray initNextIndex
    stash 0 cache
    stash 1 cache
    return ()

simulate :: IO ()
simulate = undefined
