module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum uniqueMultiples
  where
    -- Filter out factors that are 0 to avoid infinite lists
    validFactors = filter (/= 0) factors
    -- Get all multiples for each factor that are less than the limit
    allMultiples = concatMap (\factor -> [factor, factor*2..limit-1]) validFactors
    -- Remove duplicates by converting to a set
    uniqueMultiples = nub allMultiples
