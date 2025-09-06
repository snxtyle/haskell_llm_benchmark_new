module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = 
  let totalMinutes = (hour `mod` 24) * 60 + min
      normalizedMinutes = totalMinutes `mod` (24 * 60)
      normalizedHours = normalizedMinutes `div` 60
      remainingMinutes = normalizedMinutes `mod` 60
  in Clock normalizedHours remainingMinutes

toString :: Clock -> String
toString (Clock hour min) = 
  let hourStr = if hour < 10 then "0" ++ show hour else show hour
      minStr = if min < 10 then "0" ++ show min else show min
  in hourStr ++ ":" ++ minStr

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = 
  fromHourMin (h + hour) (m + min)
