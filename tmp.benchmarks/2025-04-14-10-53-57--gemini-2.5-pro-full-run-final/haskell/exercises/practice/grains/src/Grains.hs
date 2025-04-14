module Grains (square, total) where

import Data.Maybe (mapMaybe)

-- | Calculate the number of grains on a specific square.
-- Returns Nothing if the square number is invalid (not between 1 and 64).
square :: Integer -> Maybe Integer
square n
    | n >= 1 && n <= 64 = Just (2 ^ (n - 1))
    | otherwise         = Nothing

-- | Calculate the total number of grains on the chessboard.
total :: Integer
-- The total is the sum of grains on all squares from 1 to 64.
-- This is equivalent to summing 2^(n-1) for n from 1 to 64.
-- The sum of the geometric series 1 + 2 + 4 + ... + 2^63 is 2^64 - 1.
total = 2 ^ 64 - 1

-- Alternative implementation for total using the square function:
-- total = sum $ mapMaybe square [1..64]
-- Or directly:
-- total = sum [2 ^ (n - 1) | n <- [1..64]]
