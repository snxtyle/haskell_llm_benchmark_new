module Triplet (tripletsWithSum) where

-- No extra imports needed for this implementation.

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sumN
    | odd sumN = [] -- N must be even for integer solutions to a^2 + b^2 = c^2 where a+b+c=N
    | otherwise =
        let
            -- Convert sumN to Double for floating point calculations required by sqrt
            nDouble = fromIntegral sumN :: Double

            -- Calculate bounds for b based on derived inequalities:
            -- 1. a < b implies b > sumN * (1 - 1/sqrt(2))
            -- 2. b < c and a >= 1 implies b < (sumN - 1) / 2
            -- Also, a >= 1, so b must be at least 2 (since a < b).
            bMin = max 2 (floor (nDouble * (1.0 - 1.0 / sqrt 2.0)) + 1)
            bMax = floor ((nDouble - 1.0) / 2.0)

            -- Generate potential triplets by iterating through possible values of b
            -- For each b, calculate a and c using the derived formulas
            -- and filter for integer solutions.
            triplets =
                [ (a, b, c)
                | b <- [bMin .. bMax]
                , let numerator = sumN * sumN - 2 * sumN * b
                , let denominator = 2 * (sumN - b)
                -- Check if 'a' is an integer.
                -- denominator will not be zero because b < sumN/2 implies sumN - b > sumN/2 > 0.
                , numerator `mod` denominator == 0
                , let a = numerator `div` denominator
                , let c = sumN - a - b
                -- The conditions a > 0, a < b, b < c are guaranteed by the bounds of b and the derivation.
                -- The condition a*a + b*b == c*c is also guaranteed by the derivation.
                ]
        in
            triplets
