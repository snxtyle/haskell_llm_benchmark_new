module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | otherwise = map (map digitToInt) (windows n xs)
  where
    windows :: Int -> [a] -> [[a]]
    windows k ys = go ys
      where
        go zs
          | length zs < k = []
          | otherwise = take k zs : go (drop 1 zs)
