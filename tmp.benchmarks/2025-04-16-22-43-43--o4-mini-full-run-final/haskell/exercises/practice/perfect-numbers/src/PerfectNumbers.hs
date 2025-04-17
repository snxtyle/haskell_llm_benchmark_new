module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | s == n    = Just Perfect
  | s < n     = Just Deficient
  | otherwise = Just Abundant
  where
    s = sumProperDivisors n

-- Compute the sum of proper divisors of n (excluding n itself).
sumProperDivisors :: Int -> Int
sumProperDivisors 1 = 0
sumProperDivisors n = 
  let
    -- only need to check up to sqrt(n)
    root = floor . sqrt $ fromIntegral n
    -- start with 1 as a proper divisor for n > 1
    ds = foldr collect [1] [2 .. root]
    collect i acc
      | n `mod` i /= 0 = acc
      | otherwise =
          let j = n `div` i
          in if i == j
                then i : acc      -- perfect square, add only once
                else i : j : acc  -- add both divisors
  in sum ds
