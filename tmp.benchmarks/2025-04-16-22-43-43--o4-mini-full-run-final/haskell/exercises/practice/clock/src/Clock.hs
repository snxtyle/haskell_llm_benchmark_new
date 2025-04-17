module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int
  deriving Eq

-- Number of minutes in a day
dayMinutes :: Int
dayMinutes = 24 * 60

-- Normalize any minute count into [0 .. dayMinutes-1]
normalize :: Int -> Int
normalize m = let r = m `mod` dayMinutes
               in if r < 0 then r + dayMinutes else r

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute =
  Clock $ normalize (hour * 60 + minute)

toString :: Clock -> String
toString (Clock total) =
  let hour   = total `div` 60
      minute = total `mod` 60
      pad n = if n < 10 then '0' : show n else show n
  in pad hour ++ ":" ++ pad minute

addDelta :: Int -> Int -> Clock -> Clock
addDelta dHours dMinutes (Clock total) =
  Clock $ normalize (total + dHours * 60 + dMinutes)
