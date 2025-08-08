module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concatMap (multiplesOf limit) factors
  where
    multiplesOf :: Integer -> Integer -> [Integer]
    multiplesOf lim factor
      | factor <= 0 = []
      | otherwise = [factor, factor * 2 .. lim - 1]
