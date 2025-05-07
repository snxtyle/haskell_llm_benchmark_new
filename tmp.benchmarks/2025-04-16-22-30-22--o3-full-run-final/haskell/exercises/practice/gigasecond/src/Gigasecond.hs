module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime, NominalDiffTime)

-- | Add exactly one gigasecond (1 000 000 000 seconds) to the given moment.
fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigasecond
  where
    gigasecond :: NominalDiffTime
    gigasecond = 1000000000  -- 1e9 seconds
