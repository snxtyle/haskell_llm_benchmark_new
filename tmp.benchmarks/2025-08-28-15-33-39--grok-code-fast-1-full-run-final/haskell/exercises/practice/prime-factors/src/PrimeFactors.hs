module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where factors 1 _ = []
        factors m d | d * d > m = [m]
                    | m `mod` d == 0 = d : factors (m `div` d) d
                    | otherwise = factors m (d + 1)
