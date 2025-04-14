module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ removeDuplicates $ concatMap multiples (filter (> 0) factors)
  where
    multiples factor = takeWhile (< limit) [factor, factor * 2 ..]
    removeDuplicates xs = go (sort xs) []
      where
        sort [] = []
        sort (x:xs) = insert x (sort xs)
        insert x [] = [x]
        insert x (y:ys)
          | x <= y    = x : y : ys
          | otherwise = y : insert x ys
        go [] acc = reverse acc
        go [x] acc = reverse (x:acc)
        go (x:y:xs) acc
          | x == y    = go (y:xs) acc
          | otherwise = go (y:xs) (x:acc)
