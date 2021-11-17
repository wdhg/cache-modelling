module Job where

import System.Random

-- time and item ID
data Job = Job Float Int deriving (Eq, Ord)

-- TODO consider using a maclauran series expansion to approximate log
-- F(X <= x) = 1 - e^(-rate * x)
interarrivalTime :: RandomGen g => Float -> g -> (Float, g)
interarrivalTime rate gen =
  let (u, gen') = randomR (0, 1) gen
   in ((1 / rate) * log (1 / (1 - u)), gen')

pickTime :: RandomGen g => Float -> Float -> g -> (Float, g)
pickTime currentTime rate gen =
  let (timeFromNow, gen') = interarrivalTime rate gen
   in (currentTime + timeFromNow, gen')

calcRate :: Int -> Float
calcRate itemID = 1 / (1 + fromIntegral itemID)

newJob :: RandomGen g => Float -> Int -> g -> (Job, g)
newJob currentTime itemID gen =
  let rate = calcRate itemID
      (time, gen') = pickTime currentTime rate gen
   in (Job time itemID, gen')
