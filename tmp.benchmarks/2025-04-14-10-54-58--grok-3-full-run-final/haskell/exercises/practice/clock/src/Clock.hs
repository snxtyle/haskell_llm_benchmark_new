module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ ((hour + (min `div` 60)) `mod` 24) * 60 + (min `mod` 60)

toString :: Clock -> String
toString (Clock totalMinutes) = 
    let hours = (totalMinutes `div` 60) `mod` 24
        minutes = totalMinutes `mod` 60
    in padZero hours ++ ":" ++ padZero minutes
  where
    padZero n = if n < 10 then '0' : show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock totalMinutes) = 
    Clock $ (totalMinutes + hour * 60 + min) `mod` (24 * 60)
