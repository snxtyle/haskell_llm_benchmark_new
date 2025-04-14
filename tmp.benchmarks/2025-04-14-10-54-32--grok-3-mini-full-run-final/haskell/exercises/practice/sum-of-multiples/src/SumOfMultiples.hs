module SumOfMultiples (sumOfMultiples) where

import Data.List (nub, sort)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ sort $ concatMap multiples factors
  where
    multiples factor
      | factor > 0 = [x | x <- [factor, 2 * factor ..], x < limit]
      | otherwise = []  -- Ignore non-positive factors to avoid infinite loops or errors
