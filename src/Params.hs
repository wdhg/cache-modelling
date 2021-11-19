module Params where

import CacheType
import Text.JSON

data Params = Params
  { cacheType :: CacheType,
    seed :: Int,
    cacheSize :: Int,
    itemCount :: Int,
    duration :: Float
  }

instance JSON Params where
  readJSON = undefined

  showJSON params =
    JSObject $
      toJSObject
        [ ("cacheType", showJSON $ cacheType params),
          ("seed", JSRational False $ fromIntegral $ seed params),
          ("cacheSize", JSRational False $ fromIntegral $ cacheSize params),
          ("itemCount", JSRational False $ fromIntegral $ itemCount params),
          ("duration", JSRational True $ toRational $ duration params)
        ]
