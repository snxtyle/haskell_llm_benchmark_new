module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
-- Handle the case of an empty list of factors
sumOfMultiples [] _ = 0
-- Calculate the sum for non-empty lists
sumOfMultiples factors limit = sum $ nub $ concatMap multiples cleanedFactors
  where
    -- Filter out factors that are 0 to avoid infinite lists or division by zero issues.
    cleanedFactors = filter (/= 0) factors
    -- Generate multiples of a single factor below the limit
    multiples factor = takeWhile (< limit) [n * factor | n <- [1..]]
