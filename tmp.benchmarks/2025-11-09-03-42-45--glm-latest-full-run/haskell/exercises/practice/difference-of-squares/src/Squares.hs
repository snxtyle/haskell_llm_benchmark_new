module Squares (difference, squareOfSum, sumOfSquares) where

-- | Calculates the difference between the square of the sum and the sum of the squares
-- of the first N natural numbers.
difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

-- | Calculates the square of the sum of the first N natural numbers.
-- The sum of the first N natural numbers is n * (n + 1) / 2.
-- This function squares that result.
squareOfSum :: Integral a => a -> a
squareOfSum n =
  let sum_n = n * (n + 1) `div` 2
  in sum_n * sum_n

-- | Calculates the sum of the squares of the first N natural numbers.
-- This uses the formula n * (n + 1) * (2n + 1) / 6.
sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6
