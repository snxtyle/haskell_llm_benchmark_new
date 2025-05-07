module PrimeFactors (primeFactors) where

-- | Compute the prime factors of a positive integer.
--   The factors are returned in non‑decreasing order.
--
--   Examples
--
--   >>> primeFactors 1
--   []
--
--   >>> primeFactors 60
--   [2,2,3,5]
--
--   >>> primeFactors 13
--   [13]
primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []                -- 1 and non‑positives have no prime factors
  | otherwise = factor n 2
  where
    -- Recursively extract factors starting with the current divisor.
    factor :: Integer -> Integer -> [Integer]
    factor 1 _ = []
    factor m d
      | d * d > m  = [m]          -- m is prime
      | m `mod` d == 0 = d : factor (m `div` d) d
      | otherwise     = factor m (d + 1)
