module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = normalizeClock $ Clock hour min

toString :: Clock -> String
toString (Clock h m) = padZero h ++ ":" ++ padZero m
  where
    padZero n = if n < 10 then '0' : show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock h m) = normalizeClock $ Clock (h + dh) (m + dm)

normalizeClock :: Clock -> Clock
normalizeClock (Clock h m) =
  let totalMinutes = (h * 60 + m) `mod` (24 * 60)
      totalMinutes' = if totalMinutes < 0 then totalMinutes + 24 * 60 else totalMinutes
      newH = totalMinutes' `div` 60
      newM = totalMinutes' `mod` 60
  in Clock newH newM
