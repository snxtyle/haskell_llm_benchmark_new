module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    -- Helper function: factors takes the number remaining to be factored
    -- and the current potential divisor.
    factors :: Integer -> Integer -> [Integer]
    factors num divisor
      -- Base case: If the number is 1 or less, we're done factoring.
      | num <= 1                   = []
      -- Optimization: If the square of the divisor is greater than the number,
      -- the remaining number must be prime (since we've checked all smaller potential factors).
      | divisor * divisor > num    = [num]
      -- If the number is divisible by the current divisor,
      -- add the divisor to the list and continue factoring the quotient (num / divisor)
      -- with the same divisor (to catch repeated factors like in 12 = 2 * 2 * 3).
      | num `mod` divisor == 0     = divisor : factors (num `div` divisor) divisor
      -- If the number is not divisible by the current divisor,
      -- try the next divisor.
      | otherwise                  = factors num (divisor + 1)
