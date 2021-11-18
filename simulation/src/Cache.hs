{-# LANGUAGE MultiParamTypeClasses #-}

module Cache where

import Control.Monad.ST

class Cache c s where
  cachedIn :: Int -> c s -> ST s Bool
  evict :: c s -> ST s ()
  store :: Int -> c s -> ST s ()

stash :: Cache c s => Int -> c s -> ST s Bool
stash x cache = do
  alreadyCached <- cachedIn x cache
  if alreadyCached
    then return True
    else do
      evict cache
      store x cache
      return False
