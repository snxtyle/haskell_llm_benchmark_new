module Squares (difference, squareOfSum, sumOfSquares) where

-- | Compute the difference between the square of the sum and
--   the sum of the squares of the first @n@ natural numbers.
difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

-- | Compute the square of the sum of the first @n@ natural numbers.
--   Uses the closed‑form formula:
--     sum = n(n + 1) / 2
--     squareOfSum = sum²
squareOfSum :: Integral a => a -> a
squareOfSum n = s * s
  where
    s = n * (n + 1) `div` 2

-- | Compute the sum of the squares of the first @n@ natural numbers.
--   Uses the closed‑form formula:
--     sumOfSquares = n(n + 1)(2n + 1) / 6
sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6
