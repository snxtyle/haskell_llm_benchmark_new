module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ classify' n (aliquotSum n)
  where
    classify' num sum
      | sum == num = Perfect
      | sum > num  = Abundant
      | otherwise  = Deficient

aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..n `div` 2], n `mod` x == 0]
