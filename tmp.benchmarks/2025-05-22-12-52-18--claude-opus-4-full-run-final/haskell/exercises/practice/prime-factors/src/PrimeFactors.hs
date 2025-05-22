module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factorize n 2
  where
    factorize :: Integer -> Integer -> [Integer]
    factorize 1 _ = []
    factorize num divisor
      | divisor * divisor > num = [num]
      | num `mod` divisor == 0 = divisor : factorize (num `div` divisor) divisor
      | otherwise = factorize num (divisor + 1)
