module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let positiveFactors = filter (> 0) factors
      multiples f = [f, f + f .. limit - 1]
  in sum . nub . concatMap multiples $ positiveFactors
