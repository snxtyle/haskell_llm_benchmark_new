module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- | Classify a positive integer as Deficient, Perfect, or Abundant.
--   Returns Nothing for non‑positive input.
classify :: Int -> Maybe Classification
classify n
  | n <= 0          = Nothing
  | aliquot == n    = Just Perfect
  | aliquot <  n    = Just Deficient
  | otherwise       = Just Abundant
  where
    aliquot = aliquotSum n

-- | Sum of the proper divisors of a number (excluding the number itself).
aliquotSum :: Int -> Int
aliquotSum 1 = 0
aliquotSum x = sum divisors
  where
    -- 1 is always a proper divisor for x > 1.
    divisors = 1 : concatMap pairDivs [2 .. isqrt x]
    pairDivs d =
      let (q, r) = x `quotRem` d
      in if r == 0
            then if d == q
                    then [d]        -- perfect square root, include d once
                    else [d, q]     -- include both divisors
            else []
    -- Integer square root
    isqrt = floor . sqrt . fromIntegral
