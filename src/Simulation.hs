{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simulation where

import Control.Monad.Random
import Control.Monad.State

newtype Request = Request Int deriving (Eq, Show)

class (Eq a) => Cache c a where
  cachedIn :: a -> c a -> Bool
  isFull :: c a -> Bool
  stash :: a -> c a -> c a

data FIFOCache a = FIFOCache
  { size :: Int,
    contents :: [a]
  }
  deriving (Show)

instance Eq a => Cache FIFOCache a where
  x `cachedIn` cache = x `elem` contents cache

  isFull cache = size cache == length (contents cache)

  stash x cache
    | isFull cache = cache {contents = x : contents cache}
    | x `cachedIn` cache = cache
    | otherwise =
      cache {contents = x : init (contents cache)}

data Simulation :: * -> * where
  Simulation ::
    Cache c a =>
    { cache :: c a,
      hits :: Int,
      misses :: Int
    } ->
    Simulation (c a)

hit :: Cache c a => State (Simulation (c a)) ()
hit = state $ \s -> ((), s {hits = hits s + 1})

miss :: Cache c a => State (Simulation (c a)) ()
miss = state $ \s -> ((), s {misses = misses s + 1})

simulate' :: Cache c Request => RandT StdGen (State (Simulation (c Request))) Float
simulate' = do
  p <- getRandomR (0, 1) -- get random value between 0 and 1
  lift hit
  lift miss
  return p

simulate :: IO ()
simulate = undefined
