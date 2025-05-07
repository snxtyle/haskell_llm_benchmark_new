module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf) -- Used for formatting time in toString

-- The Clock data type stores time as total minutes from midnight.
-- Range: 0 to (24 * 60 - 1) = 1439 minutes.
data Clock = Clock Int
  deriving Eq -- Allows comparison of two Clock values (e.g., clock1 == clock2)

minutesInDay :: Int
minutesInDay = 24 * 60

-- Normalizes a total number of minutes to be within the range [0, minutesInDay - 1].
-- This handles rollovers (e.g., 25:00 becomes 01:00) and negative values.
normalizeMinutes :: Int -> Int
normalizeMinutes totalMinutes = (totalMinutes `mod` minutesInDay + minutesInDay) `mod` minutesInDay

-- fromHourMin creates a Clock from an hour and minute.
-- Hours and minutes can be outside typical ranges (e.g., hour 25, or minute -30)
-- and will be normalized.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  let totalMinutesFromInput = hour * 60 + min
  in Clock (normalizeMinutes totalMinutesFromInput)

-- toString converts a Clock to a string representation "HH:MM".
-- Hours and minutes are zero-padded (e.g., "08:03").
toString :: Clock -> String
toString (Clock totalMinutes) =
  let h = totalMinutes `div` 60 -- Calculate hours
      m = totalMinutes `mod` 60 -- Calculate minutes
  in printf "%02d:%02d" h m

-- addDelta adds (or subtracts) a duration (in hours and minutes) to a Clock.
addDelta :: Int -> Int -> Clock -> Clock
addDelta deltaHour deltaMin (Clock currentTotalMinutes) =
  let durationInMinutes = deltaHour * 60 + deltaMin
      newTotalMinutes = currentTotalMinutes + durationInMinutes
  in Clock (normalizeMinutes newTotalMinutes)
