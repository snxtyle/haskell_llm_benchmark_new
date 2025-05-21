module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

-- A Clock is represented by the total number of minutes past midnight.
-- There are 24 * 60 = 1440 minutes in a day.
newtype Clock = Clock Int
  deriving Eq

-- Helper function to normalize total minutes to be within [0, 1439]
-- Haskell's `mod` operator handles negative numbers correctly for clock arithmetic
-- (e.g., -10 `mod` 1440 = 1430).
normalizeMinutes :: Int -> Int
normalizeMinutes totalMinutes = totalMinutes `mod` 1440

-- Creates a Clock from a given hour and minute.
-- The input hour and minute can be outside the normal range (0-23 for hour, 0-59 for minute),
-- and the function will normalize them.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ normalizeMinutes (hour * 60 + min)

-- Converts a Clock to its string representation in "HH:MM" format.
toString :: Clock -> String
toString (Clock totalMinutes) =
  let h = totalMinutes `div` 60
      m = totalMinutes `mod` 60
  in printf "%02d:%02d" h m

-- Adds a delta (hours and minutes) to a given Clock.
-- The resulting time is normalized to stay within the 24-hour cycle.
addDelta :: Int -> Int -> Clock -> Clock
addDelta hourDelta minDelta (Clock currentMinutes) =
  let deltaInMinutes = hourDelta * 60 + minDelta
      newTotalMinutes = currentMinutes + deltaInMinutes
  in Clock $ normalizeMinutes newTotalMinutes
