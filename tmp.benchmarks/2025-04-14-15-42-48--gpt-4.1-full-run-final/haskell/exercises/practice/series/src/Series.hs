module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0 = error "Slice length must be positive"
  | n > length xs = []
  | otherwise = [map digitToInt (take n (drop i xs)) | i <- [0 .. length xs - n]]
