module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay time = addUTCTime gigasecond time
  where
    gigasecond = 1000000000  -- 10^9 seconds
