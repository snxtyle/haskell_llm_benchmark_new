module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | aliquot < n  = Just Deficient
  | aliquot == n = Just Perfect
  | aliquot > n  = Just Abundant
  where
    aliquot = aliquotSum n

aliquotSum :: Int -> Int
aliquotSum x = sum [i | i <- [1..(x `div` 2)], x `mod` i == 0]
