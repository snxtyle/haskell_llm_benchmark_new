module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | otherwise = map (map digitToInt) $ go xs
  where
    go s@(_:rest)
      | length s >= n = take n s : go rest
      | otherwise = []
    go [] = []
