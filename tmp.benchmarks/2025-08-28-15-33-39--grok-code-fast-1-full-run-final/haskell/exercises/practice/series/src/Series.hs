module Series (slices) where

import Data.List (tails)
import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | otherwise = map (map digitToInt . take n) $ takeWhile ((>= n) . length) $ tails xs
