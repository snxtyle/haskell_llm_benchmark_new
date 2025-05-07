module Squares (difference, squareOfSum, sumOfSquares) where

-- | Compute the difference between the square of the sum and the
--   sum of the squares of the first @n@ natural numbers.
--
--   This implementation uses the closed‑form formulae:
--
--   • Sum of the first @n@ natural numbers:
--       S₁ = n (n + 1) / 2
--
--   • Sum of the squares of the first @n@ natural numbers:
--       S₂ = n (n + 1) (2n + 1) / 6
--
--   Using these makes the functions run in O(1) time and O(1) space.
difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

-- | Square of the sum of the first @n@ natural numbers.
squareOfSum :: Integral a => a -> a
squareOfSum n = s * s
  where
    s = n * (n + 1) `div` 2

-- | Sum of the squares of the first @n@ natural numbers.
sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6
