module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

-- A gigasecond is 10^9 seconds.
gigasecond :: NominalDiffTime
gigasecond = fromInteger 1000000000

fromDay :: UTCTime -> UTCTime
fromDay moment = addUTCTime gigasecond moment
