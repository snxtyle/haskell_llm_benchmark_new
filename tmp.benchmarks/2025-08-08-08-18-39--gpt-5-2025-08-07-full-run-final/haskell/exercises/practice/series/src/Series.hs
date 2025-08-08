module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0 = error "slices: requested length must be positive"
  | n > length xs = []
  | otherwise =
      let ds = map digitToInt xs
       in [take n (drop i ds) | i <- [0 .. length ds - n]]
