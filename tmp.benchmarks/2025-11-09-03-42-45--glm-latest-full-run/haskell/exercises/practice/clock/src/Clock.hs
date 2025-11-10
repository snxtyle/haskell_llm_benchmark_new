module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

-- | A clock that stores the time as minutes since midnight.
-- Using a newtype is efficient and makes the logic simpler.
newtype Clock = Clock Int
  deriving (Eq) -- Two clocks are equal if their total minutes are equal.

-- The number of minutes in a 24-hour day.
minutesPerDay :: Int
minutesPerDay = 24 * 60

-- Normalizes a minute count to be within the range of a single day (0 to 1439).
-- Haskell's `mod` works correctly for both positive and negative inputs.
normalizeMinutes :: Int -> Int
normalizeMinutes m = m `mod` minutesPerDay

-- | Constructs a @Clock@ from a given hour and minute.
-- Handles values outside the normal 0-23 hour and 0-59 minute ranges by wrapping around.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  let totalMins = hour * 60 + min
  in Clock (normalizeMinutes totalMins)

-- | Converts a @Clock@ to its string representation in "HH:MM" format.
-- The hours and minutes are zero-padded.
toString :: Clock -> String
toString (Clock totalMins) =
  let h = totalMins `div` 60
      m = totalMins `mod` 60
  in printf "%02d:%02d" h m

-- | Adds a time delta (hours and minutes) to a given @Clock@.
-- Handles both positive and negative deltas.
addDelta :: Int -> Int -> Clock -> Clock
addDelta deltaHour deltaMin (Clock currentMins) =
  let deltaMins = deltaHour * 60 + deltaMin
      newMins = currentMins + deltaMins
  in Clock (normalizeMinutes newMins)
