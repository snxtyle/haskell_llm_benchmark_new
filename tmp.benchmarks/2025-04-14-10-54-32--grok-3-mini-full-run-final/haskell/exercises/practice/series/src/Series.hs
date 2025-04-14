module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 || length xs < n = []  -- Return empty list if n is negative or string is too short
  | otherwise = [ map digitToInt (take n (drop i xs)) | i <- [0 .. length xs - n] ]
