module Grains (square, total) where

-- | Calculate the number of grains on a given square.
--   Squares are numbered from 1 to 64.
--   Returns Nothing for inputs outside this range.
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | Total number of grains on the whole chessboard.
--   This is the sum of a geometric series and equals (2^64) - 1.
total :: Integer
total = 2 ^ (64 :: Integer) - 1
