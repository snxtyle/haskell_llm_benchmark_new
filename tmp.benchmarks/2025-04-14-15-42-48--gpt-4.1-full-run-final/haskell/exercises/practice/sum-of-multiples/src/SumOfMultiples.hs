module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    let validFactors = filter (/= 0) factors
        multiplesFor f = takeWhile (< limit) [f, f*2 ..]
        allMultiples = concatMap multiplesFor validFactors
        uniqueMultiples = nub allMultiples
    in sum uniqueMultiples
