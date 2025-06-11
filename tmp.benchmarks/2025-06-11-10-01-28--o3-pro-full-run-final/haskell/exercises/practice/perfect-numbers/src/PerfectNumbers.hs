module PerfectNumbers (classify, Classification(..)) where

-- | A number's classification according to its aliquot sum.
data Classification = Deficient | Perfect | Abundant
    deriving (Eq, Show)

-- | Classify a positive integer as Deficient, Perfect, or Abundant.
--   Returns Nothing for non-positive input.
classify :: Int -> Maybe Classification
classify n
    | n <= 0    = Nothing
    | otherwise = Just $ case compare aliquot n of
        LT -> Deficient
        EQ -> Perfect
        GT -> Abundant
  where
    aliquot = aliquotSum n

-- | Calculate the aliquot (proper divisor) sum of a positive integer.
--   For n == 1 the result is 0, as 1 has no proper divisors.
aliquotSum :: Int -> Int
aliquotSum 1 = 0
aliquotSum n = 1 + foldr addPair 0 [2 .. upper]
  where
    upper = floor . sqrt $ fromIntegral n
    addPair i acc
        | n `mod` i /= 0 = acc
        | otherwise      =
            let j = n `div` i
            in if i == j
                  then acc + i           -- perfect square divisor
                  else acc + i + j       -- pair of divisors
