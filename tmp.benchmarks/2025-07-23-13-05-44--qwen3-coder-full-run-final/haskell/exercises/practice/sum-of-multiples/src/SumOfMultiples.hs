module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ uniqueMultiples factors limit

uniqueMultiples :: [Integer] -> Integer -> [Integer]
uniqueMultiples factors limit = 
  let allMultiples = concatMap (multiplesBelow limit) factors
  in removeDuplicates allMultiples

multiplesBelow :: Integer -> Integer -> [Integer]
multiplesBelow limit factor
  | factor <= 0 = []
  | otherwise = takeWhile (< limit) [factor, factor * 2 ..]

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
