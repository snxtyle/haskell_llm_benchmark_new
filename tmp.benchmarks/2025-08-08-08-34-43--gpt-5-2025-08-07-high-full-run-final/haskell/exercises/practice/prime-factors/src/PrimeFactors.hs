module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2     = []
  | otherwise = go n 2
  where
    go m f
      | f * f > m      = [m]
      | m `mod` f == 0 = f : go (m `div` f) f
      | f == 2         = go m 3
      | otherwise      = go m (f + 2)
