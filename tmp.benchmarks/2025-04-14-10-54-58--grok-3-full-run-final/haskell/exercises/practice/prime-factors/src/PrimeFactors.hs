module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n 
    | n <= 1 = []
    | otherwise = factor n 2
  where
    factor num divisor
        | num == divisor = [num]
        | num `mod` divisor == 0 = divisor : factor (num `div` divisor) divisor
        | otherwise = factor num (nextDivisor divisor)
    nextDivisor 2 = 3
    nextDivisor d = d + 2
