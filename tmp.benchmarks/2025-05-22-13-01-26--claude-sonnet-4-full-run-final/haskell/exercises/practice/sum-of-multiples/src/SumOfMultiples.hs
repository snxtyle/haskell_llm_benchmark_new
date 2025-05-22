module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concatMap (multiplesBelow limit) validFactors
  where
    validFactors = filter (> 0) factors
    multiplesBelow lim factor = takeWhile (< lim) [factor, factor * 2 ..]
