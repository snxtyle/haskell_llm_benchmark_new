module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

-- | One gigasecond expressed as a 'NominalDiffTime'.
gigasecond :: NominalDiffTime
gigasecond = 1000000000  -- 10^9 seconds

-- | Add exactly one gigasecond to the supplied moment.
fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigasecond
