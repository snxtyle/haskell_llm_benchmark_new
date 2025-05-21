module Grains (square, total) where

-- | Calculates the number of grains on a specific square of the chessboard.
-- Returns 'Just' the number of grains if the square number is between 1 and 64 (inclusive).
-- Returns 'Nothing' otherwise.
square :: Integer -> Maybe Integer
square n
    | n >= 1 && n <= 64 = Just (2 ^ (n - 1))
    | otherwise         = Nothing

-- | Calculates the total number of grains on all 64 squares of the chessboard.
total :: Integer
total = 2 ^ 64 - 1
