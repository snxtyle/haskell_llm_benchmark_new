module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving Eq

dayMinutes :: Int
dayMinutes = 24 * 60

norm :: Int -> Int
norm x = x `mod` dayMinutes

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock $ norm (hour * 60 + minute)

toString :: Clock -> String
toString (Clock total) =
  let (h, m) = total `divMod` 60
      pad2 n = let s = show n in if n < 10 then '0' : s else s
  in pad2 h ++ ":" ++ pad2 m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock total) = Clock $ norm (total + hour * 60 + minute)
