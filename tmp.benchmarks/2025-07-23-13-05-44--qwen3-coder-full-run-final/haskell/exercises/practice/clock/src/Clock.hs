module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour * 60 + min) `mod` (24 * 60))

toString :: Clock -> String
toString (Clock minutes) = 
  let hours = minutes `div` 60
      mins = minutes `mod` 60
  in pad2 hours ++ ":" ++ pad2 mins
  where
    pad2 n = if n < 10 then "0" ++ show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) = 
  Clock ((currentMinutes + hour * 60 + min) `mod` (24 * 60))
