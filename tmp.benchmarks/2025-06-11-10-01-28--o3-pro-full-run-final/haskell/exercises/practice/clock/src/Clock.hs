module Clock (addDelta, fromHourMin, toString) where

-- | A clock represented as the amount of minutes since midnight.
--   The value is always normalised to the range 0-1439.
newtype Clock = Clock Int
  deriving Eq

-- | Normalise a number of minutes to the range 0-1439.
normalize :: Int -> Int
normalize m =
  let minutesPerDay = 24 * 60
      r             = m `mod` minutesPerDay  -- 'mod' keeps the sign of the dividend
  in if r < 0 then r + minutesPerDay else r

-- | Create a clock given an hour and minute value.
--   The inputs may be outside their usual ranges (negative or >59),
--   in which case they are wrapped around the 24-hour day.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock (normalize (hour * 60 + minute))

-- | Convert a clock to its zero-padded 24-hour string representation, e.g. "08:03".
toString :: Clock -> String
toString (Clock totalMinutes) =
  let (h, m) = totalMinutes `divMod` 60
      pad2 n = if n < 10 then '0' : show n else show n
  in pad2 h ++ ":" ++ pad2 m

-- | Add a duration (hours and minutes) to the given clock.
--   The result is wrapped around the 24-hour day.
addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock current) = Clock (normalize (current + dh * 60 + dm))

-- An optional Show instance that delegates to 'toString'.
instance Show Clock where
  show = toString
