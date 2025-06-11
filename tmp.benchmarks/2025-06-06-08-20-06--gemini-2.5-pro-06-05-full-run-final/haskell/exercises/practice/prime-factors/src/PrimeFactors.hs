module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors :: Integer -> Integer -> [Integer]
    factors num _ | num < 2 = []
    factors num d
      | d * d > num        = [num]
      | num `mod` d == 0   = d : factors (num `div` d) d
      | otherwise          = factors num (d + 1)
