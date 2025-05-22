module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = replicate (length xs + 1) []
  | n > length xs = []
  | otherwise = map (map digitToInt) (takeSubstrings n xs)
  where
    takeSubstrings :: Int -> String -> [String]
    takeSubstrings len str
      | len > length str = []
      | otherwise = take len str : takeSubstrings len (tail str)
