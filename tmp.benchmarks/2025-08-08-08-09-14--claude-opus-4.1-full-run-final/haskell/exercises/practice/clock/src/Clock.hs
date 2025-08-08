module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int
  deriving Eq

-- Normalize minutes to be within 0-1439 (24 hours * 60 minutes - 1)
normalize :: Int -> Int
normalize minutes = ((minutes `mod` 1440) + 1440) `mod` 1440

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (normalize (hour * 60 + min))

toString :: Clock -> String
toString (Clock totalMinutes) = 
  let normalizedMinutes = normalize totalMinutes
      hours = normalizedMinutes `div` 60
      minutes = normalizedMinutes `mod` 60
      pad n = if n < 10 then '0' : show n else show n
  in pad hours ++ ":" ++ pad minutes

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) = 
  Clock (normalize (currentMinutes + hour * 60 + min))
