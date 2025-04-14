module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing  -- Only positive integers are classified
  | aliquotSum == n = Just Perfect
  | aliquotSum > n  = Just Abundant
  | otherwise       = Just Deficient
  where
    aliquotSum = sum [x | x <- [1..n-1], n `mod` x == 0]  -- Sum of proper divisors
