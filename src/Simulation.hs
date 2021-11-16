{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Simulation where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import PQueue
import System.Random

-- time and process ID
data Job = Job Float Int deriving (Eq, Ord)

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

data Simulation :: * -> * where
  Simulation ::
    { cache :: FIFOCache s,
      getHits :: STRef s Int,
      getMisses :: STRef s Int,
      getArrivals :: STRef s (PQueue Job),
      getTimeLeft :: STRef s Float,
      getGen :: STRef s StdGen
    } ->
    Simulation s

newSimulation :: Int -> Float -> FIFOCache s -> ST s (Simulation s)
newSimulation seed duration cache = do
  initHits <- newSTRef 0
  initMisses <- newSTRef 0
  initArrivals <- newSTRef Nil
  initTime <- newSTRef duration
  initGen <- newSTRef (mkStdGen seed)
  return $ Simulation cache initHits initMisses initArrivals initTime initGen

data Stats = Stats
  { hits :: Int,
    misses :: Int,
    duration :: Float
  }
  deriving (Show)

outputStats :: Float -> Simulation s -> ST s Stats
outputStats duration state = do
  hits <- readSTRef $ getHits state
  misses <- readSTRef $ getMisses state
  return $ Stats hits misses duration

ended :: Simulation s -> ST s Bool
ended state = do
  time <- readSTRef $ getTimeLeft state
  return $ time <= 0

-- TODO consider using a maclauran series expansion to approximate log
-- F(X <= x) = 1 - e^(-rate * x)
interarrivalTime :: (Floating a, Random a) => a -> Simulation s -> ST s a
interarrivalTime rate state = do
  gen <- readSTRef $ getGen state
  let (u, gen') = randomR (0, 1) gen
  writeSTRef (getGen state) gen'
  return ((1 / rate) * log (1 / (1 - u)))

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

calcRate :: Int -> Float
calcRate processID = 1 / (1 + fromIntegral processID)

simulateFIFO' :: Simulation s -> ST s ()
simulateFIFO' state = do
  stash 1 $ cache state
  interval <- interarrivalTime 1 state
  timeLeft <- readSTRef $ getTimeLeft state
  writeSTRef (getTimeLeft state) (timeLeft - interval)
  hasEnded <- ended state
  if hasEnded
    then return ()
    else simulateFIFO' state

simulateFIFO :: Int -> Int -> Float -> ST s Stats
simulateFIFO seed size duration = do
  cache <- newFIFO size
  state <- newSimulation seed duration cache
  simulateFIFO' state
  outputStats duration state

simulate :: IO ()
simulate = print $ runST $ simulateFIFO 0 10 100
