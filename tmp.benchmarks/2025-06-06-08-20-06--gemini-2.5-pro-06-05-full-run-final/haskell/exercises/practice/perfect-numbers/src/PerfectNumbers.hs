module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | otherwise =
        let aliquotSum = sum [x | x <- [1..n-1], n `mod` x == 0]
        in Just $ case compare aliquotSum n of
            LT -> Deficient
            EQ -> Perfect
            GT -> Abundant
