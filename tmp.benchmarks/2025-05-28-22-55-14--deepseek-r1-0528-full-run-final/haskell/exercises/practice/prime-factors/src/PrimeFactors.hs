module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = factors n 2
  where
    factors :: Integer -> Integer -> [Integer]
    factors 1 _ = []
    factors n d
      | d * d > n = [n]
      | n `mod` d == 0 = d : factors (n `div` d) d
      | otherwise = factors n (d + 1)
