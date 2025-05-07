module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)

-- | A gigasecond in seconds
gigasecond :: Num a => a
gigasecond = 10^9

-- | Add a gigasecond to the given time
fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigasecond
