module Clock (addDelta, fromHourMin, toString) where

-- Represent a clock as total minutes since 00:00, normalized to [0, 1439]
newtype Clock = Clock { minutes :: Int }
  deriving Eq

-- Normalize any integer minute count to the range [0, 1439]
normalize :: Int -> Int
normalize m =
  let day = 24 * 60
      r = m `mod` day
  in if r < 0 then r + day else r

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  let total = hour * 60 + min
  in Clock (normalize total)

toString :: Clock -> String
toString (Clock m) =
  let h = m `div` 60
      mi = m `mod` 60
      pad2 x = let s = show x in if x < 10 then '0' : s else s
  in pad2 h ++ ":" ++ pad2 mi

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock m) =
  let delta = hour * 60 + min
  in Clock (normalize (m + delta))
