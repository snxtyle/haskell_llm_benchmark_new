module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum targetSum = 
  [(a, b, c) | 
    a <- [1..(targetSum `div` 3)], 
    b <- [a+1..(targetSum `div` 2)], 
    let c = targetSum - a - b,
    c > b,
    a*a + b*b == c*c]
