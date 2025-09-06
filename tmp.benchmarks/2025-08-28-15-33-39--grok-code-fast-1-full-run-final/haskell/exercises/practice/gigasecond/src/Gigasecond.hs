module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay time = addUTCTime 1000000000 time
