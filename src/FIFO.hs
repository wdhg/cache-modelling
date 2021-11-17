{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FIFO where

import Cache
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

instance Cache FIFOCache s where
  cachedIn x cache = do
    xs <- getElems $ arrayST cache
    return (x `elem` xs)

  evict cache = do
    index <- readSTRef $ indexST cache
    (lowerBound, upperBound) <- getBounds $ arrayST cache
    if index < upperBound
      then writeSTRef (indexST cache) (succ index)
      else writeSTRef (indexST cache) lowerBound

  store x cache = do
    index <- readSTRef $ indexST cache
    writeArray (arrayST cache) index x
