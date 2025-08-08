module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ case compare (aliquotSum n) n of
      LT -> Deficient
      EQ -> Perfect
      GT -> Abundant
  where
    aliquotSum :: Int -> Int
    aliquotSum 1 = 0
    aliquotSum m = 1 + go 2 (isqrt m) 0
      where
        go :: Int -> Int -> Int -> Int
        go i lim acc
          | i > lim  = acc
          | r /= 0   = go (i + 1) lim acc
          | q == i   = go (i + 1) lim (acc + i)     -- perfect square: add i once
          | otherwise = go (i + 1) lim (acc + i + q)
          where
            (q, r) = m `quotRem` i

    isqrt :: Int -> Int
    isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
