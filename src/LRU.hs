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

instance Cache LRUCache s where
  cachedIn x cache = do
    seq <- readSTRef $ seqST cache
    return $ isJust $ elemIndexL x seq

  evict cache = do
    seq <- readSTRef $ seqST cache
    let lastIndex = length seq - 1
        seq' = deleteAt lastIndex seq
    writeSTRef (seqST cache) seq'

  store x cache = do
    seq <- readSTRef $ seqST cache
    let seq' = filter (/= x) seq
        seq'' = insertAt 0 x seq
    writeSTRef (seqST cache) seq''
