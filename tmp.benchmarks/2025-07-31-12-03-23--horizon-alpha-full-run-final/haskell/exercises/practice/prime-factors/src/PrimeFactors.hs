module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factor n 2
  where
    factor :: Integer -> Integer -> [Integer]
    factor 1 _ = []
    factor m f
      | f * f > m     = [m]                      -- m is prime
      | m `mod` f == 0 = f : factor (m `div` f) f -- keep dividing by same factor
      | f == 2         = factor m 3               -- move from 2 to 3
      | otherwise      = factor m (f + 2)         -- test only odd numbers thereafter
