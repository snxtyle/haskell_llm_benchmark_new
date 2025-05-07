module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- | Calculate the sum of factors of a number n, excluding n itself.
aliquotSum :: Int -> Int
aliquotSum n = sum [i | i <- [1..n `div` 2], n `mod` i == 0]

-- | Classify a number based on its aliquot sum.
classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing  -- Classification is only defined for positive integers.
  | otherwise = Just $ case compare (aliquotSum n) n of
                         EQ -> Perfect   -- Aliquot sum equals the number
                         GT -> Abundant  -- Aliquot sum is greater than the number
                         LT -> Deficient -- Aliquot sum is less than the number
