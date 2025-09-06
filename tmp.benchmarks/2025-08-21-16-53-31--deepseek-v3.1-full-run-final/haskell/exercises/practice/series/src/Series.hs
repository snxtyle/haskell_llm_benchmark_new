module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | otherwise = map (map digitToInt) $ takeSlices n xs

takeSlices :: Int -> String -> [String]
takeSlices n xs = [take n (drop i xs) | i <- [0..length xs - n]]
