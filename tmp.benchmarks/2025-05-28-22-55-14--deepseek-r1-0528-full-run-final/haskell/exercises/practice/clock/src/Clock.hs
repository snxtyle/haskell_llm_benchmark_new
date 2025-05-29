module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ (hour * 60 + min) `mod` 1440

toString :: Clock -> String
toString (Clock total) = 
  let hours = total `div` 60
      minutes = total `mod` 60
      pad n = if n < 10 then '0' : show n else show n
  in pad hours ++ ":" ++ pad minutes

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock current) = 
  let delta = hour * 60 + min
  in Clock $ (current + delta) `mod` 1440
