module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    | n >= 1 && n <= 64 = Just (2 ^ (n - 1))
    | otherwise         = Nothing

total :: Integer
total = sum [2 ^ x | x <- [0..63]]
-- An alternative and more direct calculation for total would be (2^64) - 1
-- total = (2 ^ 64) - 1
