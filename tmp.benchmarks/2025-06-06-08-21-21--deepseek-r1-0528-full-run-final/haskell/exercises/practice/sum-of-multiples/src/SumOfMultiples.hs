module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 
    let allMultiples = [ factor * k | factor <- factors, factor > 0, k <- [1.. (limit-1) `div` factor] ]
    in Set.foldl' (+) 0 (Set.fromList allMultiples)
