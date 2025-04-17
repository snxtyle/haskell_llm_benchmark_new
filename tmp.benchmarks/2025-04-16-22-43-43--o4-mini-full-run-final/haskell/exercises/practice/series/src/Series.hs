module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0        = []
  | n > length xs = []
  | otherwise     = [ map digitToInt (take n t)
                    | t <- tails xs
                    , length t >= n
                    ]
