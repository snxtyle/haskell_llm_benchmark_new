module Squares (difference, squareOfSum, sumOfSquares) where

-- | Calculate the difference between the square of the sum and the sum of the squares
--   of the first n natural numbers.
difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

-- | Calculate the square of the sum of the first n natural numbers.
--   Uses the formula: (n * (n + 1) / 2)^2
squareOfSum :: Integral a => a -> a
squareOfSum n = let s = n * (n + 1) `div` 2 in s * s
-- Alternatively, using floating point power might introduce precision issues for large numbers,
-- so direct multiplication (s*s) is preferred for Integral types.
-- squareOfSum n = (sum [1..n]) ^ 2 -- Less efficient version

-- | Calculate the sum of the squares of the first n natural numbers.
--   Uses the formula: n * (n + 1) * (2 * n + 1) / 6
sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6
-- sumOfSquares n = sum $ map (^2) [1..n] -- Less efficient version
