module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing  -- Classification is only for positive integers
  | otherwise = Just $ case compare aliquotSum n of
                         EQ -> Perfect
                         GT -> Abundant
                         LT -> Deficient
  where
    aliquotSum :: Int
    aliquotSum = sumOfProperDivisors n

    sumOfProperDivisors :: Int -> Int
    -- Calculates the sum of all positive divisors of a number, excluding the number itself.
    -- For n=1, the range [1..(1-1)] i.e. [1..0] is empty, so sum is 0.
    -- This is correct as 1 is deficient (aliquot sum 0 < 1).
    sumOfProperDivisors num = sum [x | x <- [1..(num - 1)], num `mod` x == 0]
