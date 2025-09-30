module Clock (addDelta, fromHourMin, toString) where

-- Clock stores time as total minutes from midnight (0-1439)
newtype Clock = Clock Int
  deriving Eq

-- Normalize minutes to be within 0-1439 (24 hours * 60 minutes)
normalize :: Int -> Int
normalize mins = mins `mod` 1440

-- Create a Clock from hour and minute values
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ normalize (hour * 60 + min)

-- Convert Clock to string format "HH:MM"
toString :: Clock -> String
toString (Clock totalMins) = 
  let normalizedMins = normalize totalMins
      hours = normalizedMins `div` 60
      mins = normalizedMins `mod` 60
  in padZero hours ++ ":" ++ padZero mins
  where
    padZero n = if n < 10 then "0" ++ show n else show n

-- Add a delta (in hours and minutes) to a Clock
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock totalMins) = 
  Clock $ normalize (totalMins + hour * 60 + min)
