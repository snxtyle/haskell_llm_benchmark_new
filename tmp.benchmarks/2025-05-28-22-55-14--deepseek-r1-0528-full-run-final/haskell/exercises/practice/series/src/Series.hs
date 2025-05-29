module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | otherwise = map (ToIntToInt) (take (length xs - n + 1) (map (take n) (tails xs)))
