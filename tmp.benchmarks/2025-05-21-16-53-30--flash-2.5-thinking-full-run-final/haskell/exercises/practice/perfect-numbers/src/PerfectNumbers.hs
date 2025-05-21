module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0    = Nothing -- Non-positive integers do not have a classification in this scheme
    | otherwise = Just $ classifyPositive n

-- Helper function to classify positive integers
classifyPositive :: Int -> Classification
classifyPositive n =
    let
        -- Calculate the sum of proper divisors (aliquot sum).
        -- We iterate from 1 up to the square root of n to find divisors efficiently.
        -- If 'i' is a divisor, then 'n `div` i' is also a divisor.
        -- We need to handle the case where 'i * i == n' (perfect square)
        -- to avoid double-counting 'i'.
        -- We also need to exclude 'n' itself from the sum, as the aliquot sum
        -- is defined as the sum of factors *not including the number itself*.
        sumOfProperDivisors = sum [ d
                                  | i <- [1 .. floor (sqrt (fromIntegral n :: Double))],
                                    n `mod` i == 0,
                                    let d1 = i,
                                    let d2 = n `div` i,
                                    d <- if d1 == d2 then [d1] else [d1, d2],
                                    d /= n
                                  ]
    in
        case compare sumOfProperDivisors n of
            LT -> Deficient
            EQ -> Perfect
            GT -> Abundant
