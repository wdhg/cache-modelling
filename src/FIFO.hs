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

newFIFOCache :: Int -> ST s (FIFOCache s)
newFIFOCache size = do
  initArray <- newArray (0, size - 1) (-1) :: ST s (STArray s Int Int)
  initNextIndex <- newSTRef 0
  return $ FIFOCache initArray initNextIndex

cachedIn :: Int -> FIFOCache s -> ST s Bool
cachedIn x cache = do
  xs <- getElems $ arrayST cache
  return (x `elem` xs)

evict :: FIFOCache s -> ST s ()
evict cache = do
  index <- readSTRef $ indexST cache
  (lowerBound, upperBound) <- getBounds $ arrayST cache
  if index < upperBound
    then writeSTRef (indexST cache) (succ index)
    else writeSTRef (indexST cache) lowerBound

store :: Int -> FIFOCache s -> ST s ()
store x cache = do
  index <- readSTRef $ indexST cache
  writeArray (arrayST cache) index x

instance Cache FIFOCache s where
  stash x cache = do
    alreadyCached <- cachedIn x cache
    if alreadyCached
      then return True
      else do
        evict cache
        store x cache
        return False
