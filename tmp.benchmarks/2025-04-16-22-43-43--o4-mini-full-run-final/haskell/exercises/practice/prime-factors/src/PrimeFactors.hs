module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2     = []
  | otherwise = factor n 2
  where
    factor m f
      | f * f > m      = [m]
      | m `mod` f == 0 = f : factor (m `div` f) f
      | otherwise      = factor m (f + 1)
