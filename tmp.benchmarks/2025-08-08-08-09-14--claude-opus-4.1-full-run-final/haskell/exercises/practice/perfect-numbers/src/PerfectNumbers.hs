module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ classifyByAliquotSum n (aliquotSum n)
  where
    classifyByAliquotSum num sum
      | sum < num  = Deficient
      | sum == num = Perfect
      | otherwise  = Abundant

aliquotSum :: Int -> Int
aliquotSum n = sum $ properDivisors n

properDivisors :: Int -> [Int]
properDivisors n = [x | x <- [1..n-1], n `mod` x == 0]
