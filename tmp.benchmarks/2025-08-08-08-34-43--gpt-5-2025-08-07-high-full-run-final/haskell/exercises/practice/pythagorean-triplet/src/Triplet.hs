module Triplet (tripletsWithSum) where

import Data.List (sort)

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s
  | s <= 0    = []
  | otherwise = sort
      [ order (k * (m*m - n*n), k * (2*m*n), k * (m*m + n*n))
      | m <- [2 .. mMax]
      , n <- [1 .. m - 1]
      , odd (m - n)           -- one even, one odd
      , gcd m n == 1          -- coprime => primitive
      , let base = 2 * m * (m + n)
      , base <= s
      , s `mod` base == 0
      , let k = s `div` base
      ]
  where
    mMax :: Int
    mMax = floor (sqrt (fromIntegral s / 2 :: Double))

    order :: (Int, Int, Int) -> (Int, Int, Int)
    order (x, y, z)
      | x < y     = (x, y, z)
      | otherwise = (y, x, z)
