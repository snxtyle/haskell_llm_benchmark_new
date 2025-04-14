module Gigasecond (fromDay) where

-- Import necessary types and functions from the time library
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

-- Define one gigasecond in seconds as an Integer
gigasecondInSeconds :: Integer
gigasecondInSeconds = 10 ^ (9 :: Integer) -- 1,000,000,000 seconds

-- Convert the gigasecond duration to NominalDiffTime
-- NominalDiffTime represents a time difference in seconds.
gigasecond :: NominalDiffTime
gigasecond = fromInteger gigasecondInSeconds

-- fromDay function takes a UTCTime and returns a new UTCTime
-- that is exactly one gigasecond later.
fromDay :: UTCTime -> UTCTime
fromDay startTime = addUTCTime gigasecond startTime
