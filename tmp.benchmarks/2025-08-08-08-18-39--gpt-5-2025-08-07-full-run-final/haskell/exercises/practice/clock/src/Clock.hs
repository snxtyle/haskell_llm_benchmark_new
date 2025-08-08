module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

newtype Clock = Clock Int
  deriving (Eq)

minutesPerDay :: Int
minutesPerDay = 24 * 60

normalize :: Int -> Int
normalize m = m `mod` minutesPerDay

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (normalize (hour * 60 + min))

toString :: Clock -> String
toString (Clock mins) = printf "%02d:%02d" hours minutes
  where
    hours = mins `div` 60
    minutes = mins `mod` 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock mins) = Clock (normalize (mins + hour * 60 + min))
