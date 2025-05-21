module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    let
        -- Filter out 0 from factors, as `mod 0` is an error and 0 doesn't contribute to positive multiples.
        -- If factors is empty after filtering, or if the limit is 1 or less, no multiples can be found.
        validFactors = filter (> 0) factors
    in
        if null validFactors || limit <= 1
        then 0
        else
            -- Generate a list of numbers from 1 up to (limit - 1)
            -- and include only those that are a multiple of at least one valid factor.
            sum [ n
                | n <- [1 .. limit - 1]
                , any (\f -> n `mod` f == 0) validFactors
                ]
