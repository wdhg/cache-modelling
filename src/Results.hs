module Results where

import CacheType
import Event
import Params
import Text.JSON

data Results = Results
  { events :: [Event],
    params :: Params
  }

instance JSON Results where
  readJSON = undefined

  showJSON results =
    JSObject $
      toJSObject
        [ ("events", JSArray $ map showJSON $ events results),
          ("params", showJSON $ params results)
        ]
