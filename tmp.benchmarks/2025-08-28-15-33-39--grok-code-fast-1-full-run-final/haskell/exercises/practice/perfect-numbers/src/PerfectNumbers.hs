module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ case compare aliquot n of
      EQ -> Perfect
      GT -> Abundant
      LT -> Deficient
  where aliquot = sum [d | d <- [1..n-1], n `mod` d == 0]
