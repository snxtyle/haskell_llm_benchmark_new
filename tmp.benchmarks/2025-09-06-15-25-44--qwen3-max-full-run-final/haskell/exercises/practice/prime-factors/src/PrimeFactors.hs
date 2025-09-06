module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = case factor of
    Nothing -> [n]  -- n is prime
    Just f  -> f : primeFactors (n `div` f)
  where
    factor = findFactor n 2
    findFactor num d
      | d * d > num = Nothing
      | num `mod` d == 0 = Just d
      | otherwise = findFactor num (d + 1)
