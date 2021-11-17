{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simulation where

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef
import Event
import FIFO
import Job
import PQueue
import System.Random

data SimState :: * -> * where
  SimState ::
    { cache :: FIFOCache s, -- read only (contains STRefs)
      timeLimit :: Float, -- read only
      historyST :: STRef s [Event],
      futureJobsST :: STRef s (PQueue Job),
      timeST :: STRef s Float,
      genST :: STRef s StdGen
    } ->
    SimState s

type Simulation s a = ReaderT (SimState s) (ST s) a

get :: (SimState s -> STRef s a) -> Simulation s a
get field = asks field >>= (lift . readSTRef)

getCache :: Simulation s (FIFOCache s)
getCache = asks cache

getTimeLimit :: Simulation s Float
getTimeLimit = asks timeLimit

getHistory :: Simulation s [Event]
getHistory = get historyST

getFutureJobs :: Simulation s (PQueue Job)
getFutureJobs = get futureJobsST

getTime :: Simulation s Float
getTime = get timeST

getGen :: Simulation s StdGen
getGen = get genST

set :: (SimState s -> STRef s a) -> a -> Simulation s ()
set field value = asks field >>= (\x -> lift $ writeSTRef x value)

setHistory :: [Event] -> Simulation s ()
setHistory = set historyST

setFutureJobs :: PQueue Job -> Simulation s ()
setFutureJobs = set futureJobsST

setTime :: Float -> Simulation s ()
setTime = set timeST

setGen :: StdGen -> Simulation s ()
setGen = set genST

newSimulation :: Int -> Float -> FIFOCache s -> ST s (SimState s)
newSimulation seed duration cache = do
  initHistory <- newSTRef []
  initArrivals <- newSTRef Nil
  initTime <- newSTRef 0
  initGen <- newSTRef (mkStdGen seed)
  return $ SimState cache duration initHistory initArrivals initTime initGen

ended :: Simulation s Bool
ended = do
  time <- getTime
  endTime <- getTimeLimit
  return $ time >= endTime

logEvent :: Event -> Simulation s ()
logEvent event = do
  events <- getHistory
  setHistory (event : events)

initialiseJob :: Int -> Simulation s ()
initialiseJob itemID = do
  currentTime <- getTime
  gen <- getGen
  let (job, gen') = newJob currentTime itemID gen
  setGen gen'
  futureJobs <- getFutureJobs
  setFutureJobs $ queue job futureJobs

completeNextJob :: Simulation s ()
completeNextJob = do
  futureJobs <- getFutureJobs
  let (Job newCurrentTime itemID, futureJobs') = dequeue futureJobs
  setTime newCurrentTime
  setFutureJobs futureJobs'
  initialiseJob itemID
  cache <- getCache
  cacheHit <- lift $ stash cache itemID
  if cacheHit
    then logEvent $ Hit itemID newCurrentTime
    else logEvent $ Miss itemID newCurrentTime

initialiseJobs :: Int -> Simulation s ()
initialiseJobs count = mapM_ initialiseJob [0 .. count - 1]

simulateFIFO' :: Simulation s ()
simulateFIFO' = do
  completeNextJob
  hasEnded <- ended
  unless hasEnded simulateFIFO'

simulateFIFO :: Int -> Simulation s [Event]
simulateFIFO itemCount = do
  initialiseJobs itemCount
  simulateFIFO'
  getHistory

simulate :: Int -> Int -> Int -> Float -> [Event]
simulate seed queueSize itemCount duration = runST $ do
  cache <- newFIFO queueSize
  state <- newSimulation seed duration cache
  runReaderT (simulateFIFO itemCount) state
