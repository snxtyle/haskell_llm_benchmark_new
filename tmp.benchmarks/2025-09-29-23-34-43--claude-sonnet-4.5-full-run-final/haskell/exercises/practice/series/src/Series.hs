module Series (slices) where

import Data.Char (digitToInt, isDigit)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | not (all isDigit xs) = []
  | otherwise = map (map digitToInt) (windows n xs)
  where
    windows :: Int -> [a] -> [[a]]
    windows size list
      | size > length list = []
      | otherwise = take size list : windows size (drop 1 list)
