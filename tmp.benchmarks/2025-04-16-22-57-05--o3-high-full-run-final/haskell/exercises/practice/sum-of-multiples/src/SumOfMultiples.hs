module SumOfMultiples (sumOfMultiples) where

-- | Calculate the sum of all unique multiples of the given factors that are
--   strictly less than the provided limit.
--
--   Example:
--     sumOfMultiples [3, 5] 20 == 78
--
--   The function handles a few edge‑cases gracefully:
--
--   * An empty list of factors yields 0.
--   * Factors that are 0 are ignored (multiples of zero would be infinite).
--   * Negative factors are treated the same as their absolute values.
--   * A non‑positive limit yields 0 because there are no positive numbers
--     strictly less than such a limit.
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit
  | limit <= 0 = 0
  | null relevantFactors = 0
  | otherwise = sum [n | n <- [1 .. limit - 1], any (\f -> n `mod` f == 0) relevantFactors]
  where
    -- Ignore zeros and use absolute values to avoid issues with negatives.
    relevantFactors :: [Integer]
    relevantFactors = map abs $ filter (/= 0) factors
