module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ classifyByAliquotSum n (aliquotSum n)

aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..n `div` 2], n `mod` x == 0]

classifyByAliquotSum :: Int -> Int -> Classification
classifyByAliquotSum n aliquot
  | aliquot == n = Perfect
  | aliquot > n  = Abundant
  | otherwise    = Deficient
