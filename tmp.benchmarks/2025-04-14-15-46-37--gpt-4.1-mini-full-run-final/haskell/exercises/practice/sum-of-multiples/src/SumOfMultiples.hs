module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let multiples = concatMap (\f -> takeWhile (< limit) [f, 2*f ..]) factors
      uniqueMultiples = Set.toList $ Set.fromList multiples
  in sum uniqueMultiples
