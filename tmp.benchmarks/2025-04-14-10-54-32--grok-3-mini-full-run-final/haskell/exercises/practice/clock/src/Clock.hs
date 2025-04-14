module Clock (addDelta, fromHourMin, toString) where

-- Define Clock as total minutes since midnight for easy arithmetic
newtype Clock = Clock Int deriving (Eq)

-- Convert hours and minutes to a Clock, normalizing to 24-hour period
fromHourMin :: Int -> Int -> Clock
fromHourMin hours minutes = Clock normalizedMinutes
  where
    totalMinutes = (hours * 60) + minutes  -- Convert to total minutes
    normalizedMinutes = mod totalMinutes 1440  -- Normalize to [0, 1439] (24 hours * 60 minutes)

-- Convert Clock to a string in "HH:MM" format with zero-padding
toString :: Clock -> String
toString (Clock totalMinutes) =
  let hours = totalMinutes `div` 60  -- Extract hours
      minutes = totalMinutes `mod` 60  -- Extract minutes
      paddedHours = pad $ hours `mod` 24  -- Ensure hours wrap to 0-23 and pad
      paddedMinutes = pad minutes  -- Pad minutes
  in paddedHours ++ ":" ++ paddedMinutes
  where
    pad n = if n < 10 then '0' : show n else show n  -- Zero-pad if less than 10

-- Add hours and minutes to a Clock and return a new normalized Clock
addDelta :: Int -> Int -> Clock -> Clock
addDelta addHours addMinutes (Clock totalMinutes) =
  fromHourMin 0 (totalMinutes + (addHours * 60 + addMinutes))  -- Add and re-normalize via fromHourMin
