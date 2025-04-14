module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = go n 2
  where
    go m d
      | m <= 1    = []  -- Base case: if m is 1 or less, return empty list
      | m `mod` d == 0 = d : go (m `div` d) d  -- d divides m, so add d and continue with the same d
      | otherwise = go m (d + 1)  -- d does not divide m, try the next d
