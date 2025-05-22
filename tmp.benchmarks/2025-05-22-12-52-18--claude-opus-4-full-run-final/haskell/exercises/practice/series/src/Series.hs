module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = replicate (length xs + 1) []
  | n > length xs = []
  | otherwise = map (map digitToInt) $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows _ [] = []
    windows size lst@(_:rest)
      | length lst < size = []
      | otherwise = take size lst : windows size rest
