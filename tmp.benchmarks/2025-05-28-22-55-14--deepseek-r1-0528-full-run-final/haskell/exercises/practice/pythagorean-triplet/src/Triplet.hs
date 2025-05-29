module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = 
    [ (a, b, c)
    | a <- [1 .. n `div` 3]
    , let b_upper = (n - a - 1) `div` 2
    , b <- [a+1 .. b_upper]
    , let c = n - a - b
    , a*a + b*b == c*c
    ]
