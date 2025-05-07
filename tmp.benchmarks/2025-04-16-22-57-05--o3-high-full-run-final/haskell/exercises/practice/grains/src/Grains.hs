module Grains (square, total) where

-- | Calculate the number of grains on the given square of a chess board.
--   Returns 'Nothing' when the square index is outside the range 1‒64.
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | The total number of grains on the whole chess board (all 64 squares).
--   This is the sum of a geometric series: 2^0 + 2^1 + … + 2^63 = 2^64 - 1.
total :: Integer
total = (2 ^ (64 :: Integer)) - 1
