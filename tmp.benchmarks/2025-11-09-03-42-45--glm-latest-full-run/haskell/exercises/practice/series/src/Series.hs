module Series (slices) where

import Data.Char (digitToInt)

-- | Returns all contiguous substrings of length `n` from a string of digits.
-- Each substring is represented as a list of integers.
slices :: Int -> String -> [[Int]]
slices n xs
  | n == 0    = [[]]
  | n < 0     = []
  | n > length xs = []
  | otherwise = [map digitToInt (take n (drop i xs)) | i <- [0..length xs - n]]
