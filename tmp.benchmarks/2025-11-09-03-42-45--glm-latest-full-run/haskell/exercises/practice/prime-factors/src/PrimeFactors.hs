module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = [] -- 1 has no prime factors, and we assume n is a natural number.
  | otherwise = go n 2
  where
    -- go is a helper function that takes the number to factor and the current divisor to try.
    go :: Integer -> Integer -> [Integer]
    go m d
      -- If the divisor squared is greater than the number, the number itself must be prime.
      | d * d > m     = [m]
      -- If the number is divisible by the divisor, we found a prime factor.
      | m `mod` d == 0 = d : go (m `div` d) d
      -- Otherwise, try the next divisor.
      | otherwise      = go m (d + 1)
