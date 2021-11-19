{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LRU where

import Cache
import Control.Monad.ST
import Data.Maybe (isJust)
import Data.STRef
import Data.Sequence
import Prelude hiding (filter, length, replicate)

data LRUCache :: * -> * where
  LRUCache ::
    { seqST :: STRef s (Seq Int)
    } ->
    LRUCache s

newLRUCache :: Int -> ST s (LRUCache s)
newLRUCache size = do
  initSeq <- newSTRef $ replicate size (-1)
  return $ LRUCache initSeq

cachedIn :: Int -> LRUCache s -> ST s Bool
cachedIn x cache = do
  seq <- readSTRef $ seqST cache
  return $ isJust $ elemIndexL x seq

evictLast :: LRUCache s -> ST s ()
evictLast cache = do
  seq <- readSTRef $ seqST cache
  let lastIndex = length seq - 1
      seq' = deleteAt lastIndex seq
  writeSTRef (seqST cache) seq'

evictElem :: Int -> LRUCache s -> ST s ()
evictElem x cache = do
  seq <- readSTRef $ seqST cache
  let seq' = filter (/= x) seq
  writeSTRef (seqST cache) seq'

store :: Int -> LRUCache s -> ST s ()
store x cache = do
  seq <- readSTRef $ seqST cache
  let seq' = insertAt 0 x seq
  writeSTRef (seqST cache) seq'

instance Cache LRUCache s where
  stash x cache = do
    alreadyCached <- cachedIn x cache
    if alreadyCached
      then do
        evictElem x cache
        store x cache
        return True
      else do
        evictLast cache
        store x cache
        return False
