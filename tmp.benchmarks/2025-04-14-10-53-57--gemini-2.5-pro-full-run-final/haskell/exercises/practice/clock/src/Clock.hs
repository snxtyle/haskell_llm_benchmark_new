module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

-- Use newtype for a type-safe wrapper around Int representing minutes since midnight.
-- Deriving Eq allows direct comparison of two Clock values.
newtype Clock = Clock Int deriving Eq

minutesInDay :: Int
minutesInDay = 24 * 60

-- Normalizes total minutes to be within the range [0, minutesInDay - 1].
-- Handles negative minutes correctly by ensuring the result is positive.
normalizeMinutes :: Int -> Int
normalizeMinutes totalMinutes = (totalMinutes `mod` minutesInDay + minutesInDay) `mod` minutesInDay

-- Creates a Clock from hours and minutes.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (normalizeMinutes (hour * 60 + min))

-- Converts a Clock to a string in "HH:MM" format.
-- Uses printf for easy zero-padding.
toString :: Clock -> String
toString (Clock totalMinutes) =
  let hours = totalMinutes `div` 60
      minutes = totalMinutes `mod` 60
  in printf "%02d:%02d" hours minutes

-- Adds a delta (in hours and minutes) to a Clock.
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) =
  let deltaMinutes = hour * 60 + min
      newTotalMinutes = currentMinutes + deltaMinutes
  in Clock (normalizeMinutes newTotalMinutes)

-- Show instance for easier debugging in GHCi (optional, but helpful)
-- instance Show Clock where
--   show = toString
