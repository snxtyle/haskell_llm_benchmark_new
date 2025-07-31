module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s =
  [ (a, b, c)
  | a <- [1 .. s `div` 3]                       -- a < b < c and a + b + c = s => a <= s/3
  , b <- [a + 1 .. (s - a - 1) `div` 2]         -- b < c and c = s - a - b => b <= (s - a - 1)/2
  , let c = s - a - b
  , a < b, b < c
  , a * a + b * b == c * c
  ]
