module PrimeFactors (primeFactors) where

-- | Compute the prime factors of a positive integer.
--   For n < 2 the list is empty.
--
--   Examples:
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
  | n < 2     = []
  | otherwise = factor n 2
  where
    factor :: Integer -> Integer -> [Integer]
    factor 1 _ = []
    factor m d
      | d * d > m     = [m]                    -- m is prime
      | m `mod` d == 0 = d : factor (m `div` d) d
      | otherwise      = factor m (nextDiv d)

    -- After checking 2, we can skip even numbers.
    nextDiv :: Integer -> Integer
    nextDiv 2 = 3
    nextDiv x = x + 2
