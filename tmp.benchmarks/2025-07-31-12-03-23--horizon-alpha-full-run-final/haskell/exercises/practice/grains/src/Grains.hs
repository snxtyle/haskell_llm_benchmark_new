module Grains (square, total) where

-- | Number of grains on a given square (1..64).
--   Returns Nothing for indices outside 1..64.
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | Total number of grains on the chessboard.
--   Sum of geometric series: 2^64 - 1
total :: Integer
total = (2 ^ 64) - 1
