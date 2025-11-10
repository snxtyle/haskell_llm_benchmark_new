module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- | Calculate the sum of the proper divisors of a number.
-- For n > 1, the largest proper divisor is n `div` 2.
-- For n = 1, the sum is 0.
aliquotSum :: Int -> Int
aliquotSum n
  | n <= 1    = 0
  | otherwise = sum [d | d <- [1..n `div` 2], n `mod` d == 0]

-- | Classify a positive integer as Perfect, Abundant, or Deficient.
-- Returns Nothing for non-positive integers.
classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ case compare (aliquotSum n) n of
                           EQ -> Perfect
                           GT -> Abundant
                           LT -> Deficient
