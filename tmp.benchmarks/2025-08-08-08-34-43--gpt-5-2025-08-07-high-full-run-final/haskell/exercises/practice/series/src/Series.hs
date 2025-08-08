module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0    = error "slice length must be positive"
  | otherwise = map (map digitToInt . take n) $ take (len - n + 1) (tails xs)
  where
    len = length xs
