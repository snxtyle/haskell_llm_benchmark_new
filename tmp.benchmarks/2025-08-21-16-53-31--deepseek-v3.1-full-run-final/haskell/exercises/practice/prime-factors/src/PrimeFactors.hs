module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors 1 _ = []
    factors num divisor
      | num `mod` divisor == 0 = divisor : factors (num `div` divisor) divisor
      | otherwise = factors num (nextDivisor divisor)
    
    nextDivisor 2 = 3
    nextDivisor n = n + 2
