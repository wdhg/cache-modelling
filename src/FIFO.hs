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
    { arrayST :: STArray s Int Int,
      indexST :: STRef s Int
    } ->
    FIFOCache s

newFIFO :: Int -> ST s (FIFOCache s)
newFIFO size = do
  initArray <- newArray (0, size - 1) (-1) :: ST s (STArray s Int Int)
  initNextIndex <- newSTRef 0
  return $ FIFOCache initArray initNextIndex

cachedIn :: Int -> FIFOCache s -> ST s Bool
cachedIn x cache = do
  xs <- getElems $ arrayST cache
  return (x `elem` xs)

getNextIndex :: FIFOCache s -> ST s Int
getNextIndex cache = do
  index <- readSTRef $ indexST cache
  (lowerBound, upperBound) <- getBounds $ arrayST cache
  if index < upperBound
    then writeSTRef (indexST cache) (succ index)
    else writeSTRef (indexST cache) lowerBound
  return index

writeToNextIndex :: FIFOCache s -> Int -> ST s ()
writeToNextIndex cache x = do
  index <- getNextIndex cache
  writeArray (arrayST cache) index x
