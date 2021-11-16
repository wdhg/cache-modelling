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
import Data.Foldable (foldr')
import Data.STRef
import PQueue
import System.Random
import Text.Printf

-- time and item ID
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

data Event = Hit Int Float | Miss Int Float

instance Show Event where
  show (Hit id time) = "[" ++ printf "%014.5f" time ++ "] HIT  " ++ show id
  show (Miss id time) = "[" ++ printf "%014.5f" time ++ "] MISS " ++ show id

data Simulation :: * -> * where
  Simulation ::
    { cache :: FIFOCache s,
      getHistory :: STRef s [Event],
      getArrivals :: STRef s (PQueue Job),
      getTime :: STRef s Float,
      getTimeLimit :: Float,
      getGen :: STRef s StdGen
    } ->
    Simulation s

newSimulation :: Int -> Float -> FIFOCache s -> ST s (Simulation s)
newSimulation seed duration cache = do
  initHistory <- newSTRef []
  initArrivals <- newSTRef Nil
  initTime <- newSTRef 0
  initGen <- newSTRef (mkStdGen seed)
  return $ Simulation cache initHistory initArrivals initTime duration initGen

ended :: Simulation s -> ST s Bool
ended state = do
  time <- readSTRef $ getTime state
  return $ time >= getTimeLimit state

-- TODO consider using a maclauran series expansion to approximate log
-- F(X <= x) = 1 - e^(-rate * x)
interarrivalTime :: Float -> Simulation s -> ST s Float
interarrivalTime rate state = do
  gen <- readSTRef $ getGen state
  let (u, gen') = randomR (0, 1) gen
  writeSTRef (getGen state) gen'
  return ((1 / rate) * log (1 / (1 - u)))

pickTime :: Float -> Simulation s -> ST s Float
pickTime rate state = do
  timeFromNow <- interarrivalTime rate state
  currentTime <- readSTRef $ getTime state
  return $ currentTime + timeFromNow

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

incrementStat :: Simulation s -> (Simulation s -> STRef s Int) -> ST s ()
incrementStat state getStat = do
  stat <- readSTRef $ getStat state
  writeSTRef (getStat state) (stat + 1)
  return ()

logEvent :: Simulation s -> (Float -> Event) -> ST s ()
logEvent state eventAt = do
  time <- readSTRef $ getTime state
  events <- readSTRef $ getHistory state
  let event = eventAt time
  writeSTRef (getHistory state) (event : events)
  return ()

stash :: Int -> Simulation s -> ST s ()
stash x state = do
  alreadyCached <- x `cachedIn` cache state
  if alreadyCached
    then logEvent state (Hit x)
    else do
      logEvent state (Miss x)
      stash' x (cache state)

calcRate :: Int -> Float
calcRate itemID = 1 / (1 + fromIntegral itemID)

getNewJob :: Simulation s -> Int -> ST s Job
getNewJob state itemID = do
  time <- pickTime (calcRate itemID) state
  return $ Job time itemID

initialiseJobs :: Simulation s -> Int -> ST s ()
initialiseJobs state processes = do
  jobs <- mapM (getNewJob state) [0 .. processes - 1]
  writeSTRef (getArrivals state) (foldr' queue Nil jobs)

completeNextJob :: Simulation s -> ST s ()
completeNextJob state = do
  arrivals <- readSTRef $ getArrivals state
  let (Job time itemID, arrivals') = dequeue arrivals
  newJob <- getNewJob state itemID
  writeSTRef (getArrivals state) (queue newJob arrivals')
  writeSTRef (getTime state) time
  stash itemID state
  return ()

simulateFIFO' :: Simulation s -> ST s ()
simulateFIFO' state = do
  completeNextJob state
  hasEnded <- ended state
  if hasEnded
    then return ()
    else simulateFIFO' state

simulateFIFO :: Int -> Int -> Int -> Float -> ST s [Event]
simulateFIFO seed queueSize processes duration = do
  cache <- newFIFO queueSize
  state <- newSimulation seed duration cache
  initialiseJobs state processes
  simulateFIFO' state
  readSTRef $ getHistory state

simulate :: IO ()
simulate = mapM_ print $ runST $ simulateFIFO 1 1024 65536 1000
