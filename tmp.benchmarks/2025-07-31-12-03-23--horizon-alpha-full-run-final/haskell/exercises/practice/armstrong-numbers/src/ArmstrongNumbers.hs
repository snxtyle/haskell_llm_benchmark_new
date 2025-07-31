module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise = n == sumPowers
  where
    digits :: Integral a => a -> [a]
    digits 0 = [0]
    digits x = reverse (go x)
      where
        go 0 = []
        go y = let (q, r) = y `quotRem` 10 in r : go q

    ds = if n == 0 then [0] else digits n
    k  = fromIntegral (length ds) :: Integer
    sumPowers =
      let pow d = fromInteger (toInteger d ^ k)
      in sum (map pow ds)
