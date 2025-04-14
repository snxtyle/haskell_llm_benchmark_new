module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime, NominalDiffTime)

fromDay :: UTCTime -> UTCTime
fromDay start = addUTCTime (1000000000 :: NominalDiffTime) start
