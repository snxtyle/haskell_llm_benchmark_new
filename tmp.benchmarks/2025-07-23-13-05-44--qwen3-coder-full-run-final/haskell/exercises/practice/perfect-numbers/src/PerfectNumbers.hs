module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | aliquotSum == n = Just Perfect
  | aliquotSum > n = Just Abundant
  | otherwise = Just Deficient
  where
    aliquotSum = sum (properDivisors n)

properDivisors :: Int -> [Int]
properDivisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]
