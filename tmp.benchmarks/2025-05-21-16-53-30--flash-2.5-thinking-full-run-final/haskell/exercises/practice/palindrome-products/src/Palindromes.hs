module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort, nub)

-- | Checks if an Integer is a palindrome.
isPalindrome :: Integer -> Bool
isPalindrome n = s == reverse s
  where s = show n

-- | Generates a list of (product, (factor1, factor2)) tuples for all
--   palindromic products within the given factor range.
generatePalindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
generatePalindromeProducts minFactor maxFactor
    | minFactor > maxFactor = [] -- Handle invalid range
    | otherwise =
        [ (i * j, (i, j))
        | i <- [minFactor .. maxFactor]
        , j <- [i .. maxFactor] -- Ensure j >= i to avoid duplicate pairs and redundant calculations
        , isPalindrome (i * j)
        ]

-- | Finds the largest palindrome product and its factors within a given range.
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
    let
        palindromesWithFactors = generatePalindromeProducts minFactor maxFactor
    in
        case palindromesWithFactors of
            [] -> Nothing -- No palindromes found in the range
            _  ->
                let
                    -- Find the maximum palindrome value
                    maxVal = maximum (map fst palindromesWithFactors)
                    -- Collect all unique factor pairs for the maximum value, sorted for consistent output
                    factors = sort . nub $ [ (f1, f2) | (val, (f1, f2)) <- palindromesWithFactors, val == maxVal ]
                in
                    Just (maxVal, factors)

-- | Finds the smallest palindrome product and its factors within a given range.
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
    let
        palindromesWithFactors = generatePalindromeProducts minFactor maxFactor
    in
        case palindromesWithFactors of
            [] -> Nothing -- No palindromes found in the range
            _  ->
                let
                    -- Find the minimum palindrome value
                    minVal = minimum (map fst palindromesWithFactors)
                    -- Collect all unique factor pairs for the minimum value, sorted for consistent output
                    factors = sort . nub $ [ (f1, f2) | (val, (f1, f2)) <- palindromesWithFactors, val == minVal ]
                in
                    Just (minVal, factors)
