module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | otherwise = map (map digitToInt) $ windows n xs
  where
    windows :: Int -> String -> [String]
    windows size str
      | length str < size = []
      | otherwise = take size str : windows size (drop 1 str)
