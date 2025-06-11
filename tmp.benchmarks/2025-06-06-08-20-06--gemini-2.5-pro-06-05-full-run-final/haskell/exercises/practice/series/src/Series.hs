module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | otherwise = [map digitToInt (take n t) | t <- take (length xs - n + 1) (tails xs)]
