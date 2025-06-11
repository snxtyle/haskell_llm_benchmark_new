module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime (1000000000 :: NominalDiffTime)
