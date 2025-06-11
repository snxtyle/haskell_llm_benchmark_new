module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n s
    | n < 0 = []
    | n == 0 = [[]]
    | otherwise = map (map digitToInt . take n) $ takeWhile (\t -> length t >= n) $ tails s
