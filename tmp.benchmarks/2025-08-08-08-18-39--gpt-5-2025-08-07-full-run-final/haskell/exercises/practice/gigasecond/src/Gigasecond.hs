module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime, secondsToNominalDiffTime)

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime (secondsToNominalDiffTime 1000000000)
