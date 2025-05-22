module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

-- Normalize hours and minutes to valid 24-hour clock format
normalize :: Int -> Int -> Clock
normalize hours minutes = Clock normalizedHours normalizedMinutes
  where
    totalMinutes = hours * 60 + minutes
    -- Handle negative minutes by using `mod` which always returns non-negative result
    normalizedTotalMinutes = totalMinutes `mod` (24 * 60)
    normalizedHours = normalizedTotalMinutes `div` 60
    normalizedMinutes = normalizedTotalMinutes `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = normalize hour min

toString :: Clock -> String
toString (Clock hours minutes) = padZero hours ++ ":" ++ padZero minutes
  where
    padZero n = if n < 10 then "0" ++ show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta deltaHours deltaMinutes (Clock hours minutes) = 
  normalize (hours + deltaHours) (minutes + deltaMinutes)
