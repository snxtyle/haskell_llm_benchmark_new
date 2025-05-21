module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 1     = error "Input must be a natural number (positive integer)."
  | n == 1    = []
  | otherwise = go n 2 []
  where
    -- Helper function to find prime factors
    -- currentN: The number currently being factored
    -- divisor: The current potential prime divisor
    -- acc: Accumulator for the prime factors found so far (in reverse order)
    go :: Integer -> Integer -> [Integer] -> [Integer]
    go currentN divisor acc
      | currentN == 1 = reverse acc -- All factors found, reverse the accumulated list
      -- Optimization: If divisor squared is greater than currentN,
      -- then currentN itself must be a prime factor (if currentN > 1)
      | divisor * divisor > currentN = reverse (currentN : acc)
      | currentN `mod` divisor == 0 = go (currentN `div` divisor) divisor (divisor : acc)
      | otherwise = go currentN (divisor + 1) acc
