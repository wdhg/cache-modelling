{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simulation where

import Control.Monad.Random
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef

-- TODO consider using a maclauran series expansion to approximate log
-- F(X <= x) = 1 - e^(-rate * x)
interarrivalTime :: (Floating a, Random a) => a -> Rand StdGen a
interarrivalTime rate = do
  u <- getRandomR (0, 1)
  return $ (1 / rate) * log (1 / (1 - u))

data FIFOCache :: * -> * where
  FIFOCache ::
    { getArray :: STArray s Int Int,
      getIndex :: STRef s Int
    } ->
    FIFOCache s

cachedIn :: Int -> FIFOCache s -> ST s Bool
cachedIn x cache = do
  xs <- getElems $ getArray cache
  return (x `elem` xs)

getNextIndex :: FIFOCache s -> ST s Int
getNextIndex cache = do
  index <- readSTRef $ getIndex cache
  (lowerBound, upperBound) <- getBounds $ getArray cache
  if index < upperBound
    then writeSTRef (getIndex cache) (succ index)
    else writeSTRef (getIndex cache) lowerBound
  return index

-- x is not in cache
stash' :: Int -> FIFOCache s -> ST s ()
stash' x cache = do
  index <- getNextIndex cache
  writeArray (getArray cache) index x
  return ()

stash :: Int -> FIFOCache s -> ST s ()
stash x cache = do
  alreadyCached <- x `cachedIn` cache
  if alreadyCached
    then return ()
    else stash' x cache

newFIFO :: Int -> ST s (FIFOCache s)
newFIFO size = do
  initArray <- newArray (0, size - 1) (-1) :: ST s (STArray s Int Int)
  initNextIndex <- newSTRef 0
  return $ FIFOCache initArray initNextIndex

simulateFIFO' :: Int -> FIFOCache s -> ST s ()
simulateFIFO' 0 cache = pure ()
simulateFIFO' x cache = do
  stash x cache
  simulateFIFO' (x - 1) cache

simulateFIFO :: Int -> IO ()
simulateFIFO size = print $
  elems $
    runSTArray $ do
      cache <- newFIFO size
      simulateFIFO' 10 cache
      return $ getArray cache

simulate :: IO ()
simulate = simulateFIFO 10
