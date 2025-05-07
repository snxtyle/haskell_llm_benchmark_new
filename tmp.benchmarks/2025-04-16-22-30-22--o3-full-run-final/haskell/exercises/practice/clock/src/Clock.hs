module Clock (addDelta, fromHourMin, toString) where

-- | Internally we store the time as the amount of minutes elapsed
--   since midnight. 0 represents 00:00, 1439 represents 23:59.
newtype Clock = Clock Int
  deriving (Eq)

-- | Ensure the number of minutes stays within the valid range [0, 1439].
normalize :: Int -> Int
normalize = (`mod` minutesPerDay)
  where
    minutesPerDay = 24 * 60

-- | Construct a clock from hour and minute values, normalising if the input
--   lies outside the usual 0‑23 / 0‑59 range.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock . normalize $ hour * 60 + minute

-- | Convert a clock to a zero‑padded “HH:MM” string.
toString :: Clock -> String
toString (Clock mins) = pad2 h ++ ":" ++ pad2 m
  where
    (h, m) = mins `divMod` 60

    pad2 :: Int -> String
    pad2 n
      | n < 10    = '0' : show n
      | otherwise = show n

-- | Add a delta, expressed in hours and minutes, to an existing clock.
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock mins) =
  Clock . normalize $ mins + hour * 60 + minute
