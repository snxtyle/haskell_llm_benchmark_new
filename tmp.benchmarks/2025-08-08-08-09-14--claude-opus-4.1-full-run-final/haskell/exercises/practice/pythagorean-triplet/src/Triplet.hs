module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum targetSum = 
    [ (a, b, c) 
    | a <- [1 .. targetSum `div` 3]  -- a must be less than sum/3 for a < b < c
    , b <- [a + 1 .. (targetSum - a) `div` 2]  -- b > a and b < c, so b < (sum - a) / 2
    , let c = targetSum - a - b
    , c > b  -- ensure a < b < c
    , a * a + b * b == c * c  -- Pythagorean theorem
    ]
