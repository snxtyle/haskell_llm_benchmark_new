module SumOfMultiples (sumOfMultiples) where

import Data.List (nub, subsequences)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 
    let factors' = nub (filter (>0) factors)
        nonEmptySubsets = tail (subsequences factors')
        term s = let k = foldl lcm 1 s
                     n = (limit - 1) `div` k
                 in k * n * (n+1) `div` 2
    in sum [ if odd (length subset) then term subset else - term subset | subset <- nonEmptySubsets ]
