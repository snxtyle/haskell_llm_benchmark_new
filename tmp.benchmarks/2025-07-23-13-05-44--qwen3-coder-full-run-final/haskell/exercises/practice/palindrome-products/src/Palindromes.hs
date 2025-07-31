module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (group, sort)

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

factorPairs :: Integer -> Integer -> Integer -> [(Integer, Integer)]
factorPairs minFactor maxFactor productVal = 
    [(i, productVal `div` i) | i <- [minFactor..maxFactor], 
                               productVal `mod` i == 0, 
                               i <= productVal `div` i, 
                               productVal `div` i >= minFactor, 
                               productVal `div` i <= maxFactor]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise = 
        let products = [i * j | i <- [minFactor..maxFactor], j <- [i..maxFactor]]
            palindromes = filter isPalindrome products
        in case palindromes of
            [] -> Nothing
            _ -> let maxPalindrome = maximum palindromes
                     factors = factorPairs minFactor maxFactor maxPalindrome
                 in Just (maxPalindrome, factors)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise = 
        let products = [i * j | i <- [minFactor..maxFactor], j <- [i..maxFactor]]
            palindromes = filter isPalindrome products
        in case palindromes of
            [] -> Nothing
            _ -> let minPalindrome = minimum palindromes
                     factors = factorPairs minFactor maxFactor minPalindrome
                 in Just (minPalindrome, factors)
