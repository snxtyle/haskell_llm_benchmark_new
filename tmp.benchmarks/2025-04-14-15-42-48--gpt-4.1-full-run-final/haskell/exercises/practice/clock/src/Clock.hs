module Clock (addDelta, fromHourMin, toString) where

-- | A Clock is represented as the number of minutes since midnight (00:00).
newtype Clock = Clock Int

-- | Normalize minutes to the range [0, 1439] (i.e., 0:00 to 23:59)
normalize :: Int -> Int
normalize mins = (mins `mod` 1440 + 1440) `mod` 1440

instance Eq Clock where
  (Clock m1) == (Clock m2) = normalize m1 == normalize m2

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ normalize (hour * 60 + min)

toString :: Clock -> String
toString (Clock mins) =
  let total = normalize mins
      h = total `div` 60
      m = total `mod` 60
  in pad2 h ++ ":" ++ pad2 m
  where
    pad2 n = if n < 10 then '0' : show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock mins) = Clock $ normalize (mins + hour * 60 + min)
