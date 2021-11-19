{-# LANGUAGE MultiParamTypeClasses #-}

module Cache where

import Control.Monad.ST

class Cache c s where
  stash :: Cache c s => Int -> c s -> ST s Bool
