module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as S

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let positiveFactors = filter (> 0) factors
      multiplesFor f = takeWhile (< limit) $ map (* f) [1 ..]
      allMultiplesSet = S.fromList $ concatMap multiplesFor positiveFactors
  in  sum allMultiplesSet
