module PrimeFactors (primeFactors) where

-- | Compute the prime factors of a given natural number.
--   For n < 2 the result is the empty list.
primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2     = []
  | otherwise = factor n 2
  where
    -- Recursively gather factors.
    factor :: Integer -> Integer -> [Integer]
    factor 1 _ = []
    factor m d
      | d * d > m     = [m]                 -- remainder is prime
      | m `mod` d == 0 = d : factor (m `div` d) d
      | otherwise      = factor m (d + 1)
