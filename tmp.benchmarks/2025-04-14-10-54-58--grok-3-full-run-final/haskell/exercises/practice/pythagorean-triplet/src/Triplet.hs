module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a, b, c) | 
    a <- [1..sum], 
    b <- [a+1..sum], 
    let c = sum - a - b, 
    c > b, 
    a * a + b * b == c * c]
