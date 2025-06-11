module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)

-- | Add exactly one gigasecond (1 000 000 000 seconds) to a given UTCTime.
fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime 1000000000
