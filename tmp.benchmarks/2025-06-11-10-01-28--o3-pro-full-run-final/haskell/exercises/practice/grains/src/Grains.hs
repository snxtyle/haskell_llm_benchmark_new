module Grains (square, total) where

-- | Calculate the number of grains on the given square of a chessboard.
--   The first square (n = 1) has 1 grain, and the number doubles on
--   each subsequent square. If the requested square is not within the
--   valid range (1‒64), return Nothing.
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | Calculate the total number of grains on the chessboard.
--   This is the sum of a geometric series with ratio 2 over 64 terms:
--     total = 2^64 - 1
total :: Integer
total = 2 ^ (64 :: Integer) - 1
