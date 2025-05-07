module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime, NominalDiffTime)

-- A gigasecond is 10^9 (1,000,000,000) seconds.
gigasecond :: NominalDiffTime
gigasecond = 1000000000

fromDay :: UTCTime -> UTCTime
fromDay time = addUTCTime gigasecond time
