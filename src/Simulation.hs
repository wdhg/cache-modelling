{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simulation where

import Cache
import CacheType
import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.ST (getElems)
import Data.STRef
import Event
import FIFO
import LRU
import PQueue
import Params
import Request
import Results
import System.Random

data SimState :: (* -> *) -> * -> * where
  SimState ::
    Cache c s =>
    { cache :: c s, -- read only (contains STRefs)
      timeLimit :: Float, -- read only
      historyST :: STRef s [Event],
      futureRequestsST :: STRef s (PQueue Request),
      timeST :: STRef s Float,
      genST :: STRef s StdGen
    } ->
    SimState c s

newSimState :: Cache c s => c s -> Int -> Float -> ST s (SimState c s)
newSimState cache seed duration = do
  initHistory <- newSTRef []
  initArrivals <- newSTRef Nil
  initTime <- newSTRef 0
  initGen <- newSTRef (mkStdGen seed)
  return $ SimState cache duration initHistory initArrivals initTime initGen

type Simulation c s a = ReaderT (SimState c s) (ST s) a

get :: (SimState c s -> STRef s a) -> Simulation c s a
get field = asks field >>= (lift . readSTRef)

getCache :: Simulation c s (c s)
getCache = asks cache

getTimeLimit :: Simulation c s Float
getTimeLimit = asks timeLimit

getHistory :: Simulation c s [Event]
getHistory = get historyST

getFutureRequests :: Simulation c s (PQueue Request)
getFutureRequests = get futureRequestsST

getTime :: Simulation c s Float
getTime = get timeST

getGen :: Simulation c s StdGen
getGen = get genST

set :: (SimState c s -> STRef s a) -> a -> Simulation c s ()
set field value = asks field >>= (\x -> lift $ writeSTRef x value)

setHistory :: [Event] -> Simulation c s ()
setHistory = set historyST

setFutureRequests :: PQueue Request -> Simulation c s ()
setFutureRequests = set futureRequestsST

setTime :: Float -> Simulation c s ()
setTime = set timeST

setGen :: StdGen -> Simulation c s ()
setGen = set genST

ended :: Simulation c s Bool
ended = do
  time <- getTime
  endTime <- getTimeLimit
  return $ time >= endTime

logEvent :: Event -> Simulation c s ()
logEvent event = do
  events <- getHistory
  setHistory (event : events)

newRequestFor :: Int -> Simulation c s ()
newRequestFor itemID = do
  currentTime <- getTime
  gen <- getGen
  let (request, gen') = newRequest currentTime itemID gen
  setGen gen'
  futureRequests <- getFutureRequests
  setFutureRequests $ queue request futureRequests

completeNextRequest :: Cache c s => Simulation c s ()
completeNextRequest = do
  futureRequests <- getFutureRequests
  let (Request newCurrentTime itemID, futureRequests') = dequeue futureRequests
  setTime newCurrentTime
  setFutureRequests futureRequests'
  newRequestFor itemID
  cache <- getCache
  cacheHit <- lift $ stash itemID cache
  if cacheHit
    then logEvent $ Hit itemID newCurrentTime
    else logEvent $ Miss itemID newCurrentTime

initialiseRequests :: Int -> Simulation c s ()
initialiseRequests count = mapM_ newRequestFor [0 .. count - 1]

simulateLoop :: Cache c s => Simulation c s ()
simulateLoop = do
  completeNextRequest
  hasEnded <- ended
  unless hasEnded simulateLoop

simulate'' :: Cache c s => Int -> Simulation c s [Event]
simulate'' itemCount = do
  initialiseRequests itemCount
  simulateLoop
  getHistory

simulate' :: Cache c s => (Int -> ST s (c s)) -> Params -> ST s [Event]
simulate' newCache params = do
  cache <- newCache $ cacheSize params
  state <- newSimState cache (seed params) (duration params)
  runReaderT (simulate'' $ itemCount params) state

simulate :: Params -> Results
simulate params = runST $ do
  events <- case cacheType params of
    FIFO -> simulate' newFIFOCache params
    LRU -> simulate' newLRUCache params
  return
    Results
      { events = events,
        params = params
      }
