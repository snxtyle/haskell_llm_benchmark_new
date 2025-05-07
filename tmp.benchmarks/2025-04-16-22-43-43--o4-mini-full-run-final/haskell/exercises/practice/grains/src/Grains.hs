module Grains (square, total) where

-- | Returns the number of grains on the nth square (1-based), if 1 ≤ n ≤ 64.
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | Total number of grains on all 64 squares.
total :: Integer
total = sum [2 ^ (i - 1) | i <- [1 .. 64]]
