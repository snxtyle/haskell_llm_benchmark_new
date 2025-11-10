module Grains (square, total) where

-- | Calculates the number of grains of wheat on a specific square of a chessboard.
--   The number of grains doubles on each successive square.
--   Returns 'Nothing' if the square number is outside the range of 1 to 64.
--   Returns 'Just' the number of grains for a valid square.
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | Calculates the total number of grains of wheat on the entire chessboard.
--   This is the sum of the geometric series 2^0 + 2^1 + ... + 2^63,
--   which is equal to 2^64 - 1.
total :: Integer
total = 2 ^ 64 - 1
