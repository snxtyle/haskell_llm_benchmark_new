module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1 = []
  | otherwise = go n 2 []
  where
    -- go currentNumber currentDivisor accumulatedFactors
    go :: Integer -> Integer -> [Integer] -> [Integer]
    go 1 _ acc = acc -- If the number is reduced to 1, we're done.
    go num divisor acc
      -- If divisor*divisor > num, then num must be prime (if it's > 1)
      -- because it wasn't divisible by any smaller primes.
      | divisor * divisor > num = acc ++ [num]
      | num `mod` divisor == 0 =
          -- Divisor is a factor, add it to acc, and continue with num / divisor
          -- and the same divisor (to catch repeated factors).
          go (num `div` divisor) divisor (acc ++ [divisor])
      | otherwise =
          -- Divisor is not a factor, try the next appropriate divisor.
          -- If current divisor is 2, next is 3. Otherwise, next is divisor + 2 (to skip even numbers).
          go num (if divisor == 2 then 3 else divisor + 2) acc
