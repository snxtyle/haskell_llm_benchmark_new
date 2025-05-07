module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- | Classify a positive integer as Deficient, Perfect, or Abundant
--   according to Nicomachus’ scheme.  Returns Nothing for non‑positive
--   input.
classify :: Int -> Maybe Classification
classify n
  | n <= 0        = Nothing
  | aliquot < n   = Just Deficient
  | aliquot == n  = Just Perfect
  | otherwise     = Just Abundant
  where
    aliquot = aliquotSum n

-- | Sum of the proper divisors of the given number.
aliquotSum :: Int -> Int
aliquotSum m = sum [d | d <- [1 .. m `div` 2], m `mod` d == 0]
