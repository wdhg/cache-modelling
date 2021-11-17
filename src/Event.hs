module Event where

import Text.Printf

data Event = Hit Int Float | Miss Int Float

instance Show Event where
  show (Hit id time) = "[" ++ printf "%013.5f" time ++ "] HIT  " ++ show id
  show (Miss id time) = "[" ++ printf "%013.5f" time ++ "] MISS " ++ show id
