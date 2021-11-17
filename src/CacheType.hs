module CacheType where

import Text.JSON

data CacheType = FIFO | LRU deriving (Show)

instance JSON CacheType where
  readJSON = undefined
  showJSON = JSString . toJSString . show
