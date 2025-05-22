module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving Eq

-- Convert hours and minutes to total minutes, normalized to 24-hour format
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (normalizeMinutes (hour * 60 + min))

-- Format clock as "HH:MM"
toString :: Clock -> String
toString (Clock totalMins) = 
  let hours = totalMins `div` 60
      mins = totalMins `mod` 60
  in padZero hours ++ ":" ++ padZero mins

-- Add delta hours and minutes to a clock
addDelta :: Int -> Int -> Clock -> Clock
addDelta deltaHour deltaMin (Clock totalMins) = 
  Clock (normalizeMinutes (totalMins + deltaHour * 60 + deltaMin))

-- Helper function to normalize minutes to 0-1439 range (24 * 60 - 1)
normalizeMinutes :: Int -> Int
normalizeMinutes mins = mins `mod` (24 * 60)

-- Helper function to pad single digits with zero
padZero :: Int -> String
padZero n = if n < 10 then "0" ++ show n else show n
