module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2     = []
  | otherwise = go n 2
  where
    go 1 _ = []
    go m f
      | f * f > m      = [m]                 -- remaining m is prime
      | m `mod` f == 0 = f : go (m `div` f) f
      | f == 2         = go m 3              -- after 2, try odd factors only
      | otherwise      = go m (f + 2)
