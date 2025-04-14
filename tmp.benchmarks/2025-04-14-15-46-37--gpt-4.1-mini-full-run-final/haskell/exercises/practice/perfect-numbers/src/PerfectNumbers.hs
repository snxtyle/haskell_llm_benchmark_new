module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ case aliquotSum n of
      s | s == n    -> Perfect
        | s > n     -> Abundant
        | otherwise -> Deficient

aliquotSum :: Int -> Int
aliquotSum n = sum $ filter (\x -> n `mod` x == 0) [1 .. n `div` 2]
