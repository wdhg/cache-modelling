module Event where

import Text.JSON
import Text.Printf

data Event = Hit Int Float | Miss Int Float

instance Show Event where
  show (Hit id time) = "[" ++ printf "%013.5f" time ++ "] HIT  " ++ show id
  show (Miss id time) = "[" ++ printf "%013.5f" time ++ "] MISS " ++ show id

instance JSON Event where
  readJSON = undefined

  showJSON (Hit itemID time) =
    JSObject $
      toJSObject
        [ ("hit", JSBool True),
          ("item", JSRational False $ fromIntegral itemID),
          ("time", JSRational True $ toRational time)
        ]
  showJSON (Miss itemID time) =
    JSObject $
      toJSObject
        [ ("hit", JSBool False),
          ("item", JSRational False $ fromIntegral itemID),
          ("time", JSRational True $ toRational time)
        ]
