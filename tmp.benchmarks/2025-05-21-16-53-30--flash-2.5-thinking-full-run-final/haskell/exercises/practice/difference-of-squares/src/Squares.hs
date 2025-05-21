module Squares (difference, squareOfSum, sumOfSquares) where

-- | Calculates the square of the sum of the first n natural numbers.
-- Formula: (n * (n + 1) / 2)^2
squareOfSum :: Integral a => a -> a
squareOfSum n =
    let sumN = n * (n + 1) `div` 2
    in sumN * sumN

-- | Calculates the sum of the squares of the first n natural numbers.
-- Formula: n * (n + 1) * (2*n + 1) / 6
sumOfSquares :: Integral a => a -> a
sumOfSquares n =
    n * (n + 1) * (2 * n + 1) `div` 6

-- | Calculates the difference between the square of the sum and the sum of the squares
-- of the first n natural numbers.
-- Formula: squareOfSum n - sumOfSquares n
difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n
