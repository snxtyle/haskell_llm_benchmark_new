module SumOfMultiples (sumOfMultiples) where

import Data.List (nub) -- Used for removing duplicate multiples

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    -- If the limit is 1 or less, no multiples will be strictly less than it.
    if limit <= 1 then 0
    else
        let
            -- Consider only positive factors. Base values are typically positive.
            -- Factors of 0 or negative numbers don't generate multiples in the way
            -- described (e.g. multiples of 3 are 3, 6, 9...).
            positiveFactors = filter (> 0) factors

            -- Generates multiples of a single number 'n' strictly less than 'lim'.
            -- Example: multiplesOfFactor 3 20 -> [3, 6, 9, 12, 15, 18]
            multiplesOfFactor :: Integer -> Integer -> [Integer]
            multiplesOfFactor n lim = takeWhile (< lim) [n * x | x <- [1..]]

            -- Generate all multiples for all positive factors, then concatenate them.
            -- Example: factors = [3, 5], limit = 20
            -- multiples for 3: [3, 6, 9, 12, 15, 18]
            -- multiples for 5: [5, 10, 15]
            -- allMultiples: [3, 6, 9, 12, 15, 18, 5, 10, 15]
            allMultiples :: [Integer]
            allMultiples = concatMap (\factor -> multiplesOfFactor factor limit) positiveFactors

            -- Remove duplicates from the list of all multiples.
            -- Example: nub [3, 6, 9, 12, 15, 18, 5, 10, 15] -> [3, 6, 9, 12, 15, 18, 5, 10]
            -- (Order depends on nub's behavior, but content is what matters for sum)
            uniqueMultiplesList :: [Integer]
            uniqueMultiplesList = nub allMultiples
        in
            -- Calculate the sum of the unique multiples.
            -- Example: sum [3, 6, 9, 12, 15, 18, 5, 10] -> 78
            sum uniqueMultiplesList
