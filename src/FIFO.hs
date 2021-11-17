{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module FIFO where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import Event

data FIFOCache :: * -> * where
  FIFOCache ::
    { getArray :: STArray s Int Int,
      getIndex :: STRef s Int
    } ->
    FIFOCache s

newFIFO :: Int -> ST s (FIFOCache s)
newFIFO size = do
  initArray <- newArray (0, size - 1) (-1) :: ST s (STArray s Int Int)
  initNextIndex <- newSTRef 0
  return $ FIFOCache initArray initNextIndex

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

writeToNextIndex :: FIFOCache s -> Int -> ST s ()
writeToNextIndex cache x = do
  index <- getNextIndex cache
  writeArray (getArray cache) index x
