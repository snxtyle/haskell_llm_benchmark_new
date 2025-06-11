module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub . concatMap multiplesUnderLimit $ cleanFactors
  where
    -- The problem implies positive base values. We filter out non-positive
    -- numbers to be safe and avoid issues with 0.
    cleanFactors = filter (> 0) factors

    -- For a given factor, generates all its multiples that are less than the limit.
    multiplesUnderLimit fact = takeWhile (< limit) [fact, fact * 2 ..]
