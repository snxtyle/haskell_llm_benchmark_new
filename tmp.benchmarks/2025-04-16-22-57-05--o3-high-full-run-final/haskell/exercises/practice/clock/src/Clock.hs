module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

-- | A clock is represented as the number of minutes elapsed since midnight.
--   The value is always kept in the range [0, 1440), i.e. 0‑23:59.
newtype Clock = Clock Int
  deriving Eq

-- Provide a useful textual representation, reusing `toString`.
instance Show Clock where
  show = toString

-- | Create a clock given an hour and minute, normalising any overflow
--   (or underflow) into the valid 24‑hour range.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock (normalise totalMinutes)
  where
    totalMinutes = hour * 60 + minute

-- | Convert a clock back into its zero‑padded "HH:MM" textual form.
toString :: Clock -> String
toString (Clock mins) = printf "%02d:%02d" h m
  where
    (h, m) = mins `divMod` 60

-- | Add a duration (expressed in hours and minutes) to a clock,
--   returning the resulting normalised clock.
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock mins) = Clock $ normalise $
                                    mins + hour * 60 + minute

-- Internal helpers -----------------------------------------------------------

minutesPerDay :: Int
minutesPerDay = 24 * 60

-- | Ensure a minute count is within [0, minutesPerDay).
normalise :: Int -> Int
normalise m = (m `mod` minutesPerDay + minutesPerDay) `mod` minutesPerDay
