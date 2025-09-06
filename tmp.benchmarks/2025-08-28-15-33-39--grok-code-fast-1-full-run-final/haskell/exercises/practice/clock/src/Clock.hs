module Clock (Clock, addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour * 60 + min) `mod` 1440)

toString :: Clock -> String
toString (Clock mins) = let h = mins `div` 60
                            m = mins `mod` 60
                            pad n = if n < 10 then '0' : show n else show n
                        in pad h ++ ":" ++ pad m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock mins) = Clock ((mins + hour * 60 + min) `mod` 1440)
