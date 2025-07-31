module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor 1 _ = []
    factor num divisor
      | divisor * divisor > num = [num]
      | num `mod` divisor == 0 = divisor : factor (num `div` divisor) divisor
      | otherwise = factor num (divisor + 1)
