module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | s == n    = Just Perfect
  | s > n     = Just Abundant
  | otherwise = Just Deficient
  where
    s = aliquotSum n

-- Compute the sum of proper divisors (aliquot sum) of a positive integer.
aliquotSum :: Int -> Int
aliquotSum n
  | n <= 1    = 0
  | otherwise = go 2 (isqrt n) 1
  where
    go :: Int -> Int -> Int -> Int
    go i end acc
      | i > end  = acc
      | otherwise =
          let (q, r) = n `quotRem` i
          in if r == 0
               then
                 let acc' = acc + i + (if i /= q then q else 0)
                 in go (i + 1) end acc'
               else go (i + 1) end acc

-- Integer square root
isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral
