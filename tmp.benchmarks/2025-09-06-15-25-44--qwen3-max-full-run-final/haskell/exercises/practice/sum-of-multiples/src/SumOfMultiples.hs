module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ foldr union [] multiples
  where
    multiples = map (multiplesLessThan limit) (filter (> 0) factors)
    multiplesLessThan l f = [f, f*2 .. l-1]
    union [] ys = ys
    union (x:xs) ys
      | x `elem` ys = union xs ys
      | otherwise = x : union xs ys
