module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ classifyByAliquotSum n (aliquotSum n)
  where
    aliquotSum :: Int -> Int
    aliquotSum x = sum [d | d <- [1..x-1], x `mod` d == 0]
    
    classifyByAliquotSum :: Int -> Int -> Classification
    classifyByAliquotSum num sumOfDivisors
      | sumOfDivisors < num  = Deficient
      | sumOfDivisors == num = Perfect
      | otherwise            = Abundant
