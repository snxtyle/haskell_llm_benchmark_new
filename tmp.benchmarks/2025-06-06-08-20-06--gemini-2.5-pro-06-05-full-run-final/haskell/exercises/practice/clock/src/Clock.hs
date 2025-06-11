module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = Clock Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ (hour * 60 + min) `mod` (24 * 60)

toString :: Clock -> String
toString (Clock totalMinutes) = printf "%02d:%02d" h m
  where
    h = totalMinutes `div` 60
    m = totalMinutes `mod` 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) = fromHourMin 0 (currentMinutes + hour * 60 + min)
