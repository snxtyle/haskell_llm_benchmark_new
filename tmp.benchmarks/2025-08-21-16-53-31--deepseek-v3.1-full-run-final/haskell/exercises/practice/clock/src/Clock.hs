module Clock (addDelta, fromHourMin, toString, Clock) where

newtype Clock = Clock { totalMinutes :: Int }
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ normalize (hour * 60 + min)
  where
    normalize minutes = (minutes `mod` 1440 + 1440) `mod` 1440

toString :: Clock -> String
toString (Clock minutes) =
  let hours = minutes `div` 60 `mod` 24
      mins = minutes `mod` 60
  in padZero (show hours) ++ ":" ++ padZero (show mins)
  where
    padZero s = if length s == 1 then "0" ++ s else s

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock current) = fromHourMin 0 (current + hour * 60 + min)
