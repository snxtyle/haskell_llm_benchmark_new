module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum . nub $ concat [ [f, 2*f .. limit - 1] | f <- factors, f > 0 ]
