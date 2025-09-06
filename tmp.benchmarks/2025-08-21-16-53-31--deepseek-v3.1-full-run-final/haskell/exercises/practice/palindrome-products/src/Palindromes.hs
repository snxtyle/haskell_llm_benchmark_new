module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub)

-- Helper function to check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- Find all palindromic products and their factors in the given range
findPalindromicProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
findPalindromicProducts minFactor maxFactor = 
    [(product, (x, y)) | 
        x <- [minFactor..maxFactor],
        y <- [x..maxFactor],
        let product = x * y,
        isPalindrome product]

-- Group factors by their product
groupFactors :: [(Integer, (Integer, Integer))] -> [(Integer, [(Integer, Integer)])]
groupFactors = foldr (\(p, factors) acc -> 
    case lookup p acc of
        Just existing -> (p, factors : existing) : filter (\(p', _) -> p' /= p) acc
        Nothing -> (p, [factors]) : acc) []

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | null palindromes = Nothing
    | otherwise = Just (maxPalindrome, factorsForMax)
    where
        products = findPalindromicProducts minFactor maxFactor
        grouped = groupFactors products
        palindromes = map fst grouped
        maxPalindrome = maximum palindromes
        factorsForMax = case lookup maxPalindrome grouped of
            Just factors -> factors
            Nothing -> []

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | null palindromes = Nothing
    | otherwise = Just (minPalindrome, factorsForMin)
    where
        products = findPalindromicProducts minFactor maxFactor
        grouped = groupFactors products
        palindromes = map fst grouped
        minPalindrome = minimum palindromes
        factorsForMin = case lookup minPalindrome grouped of
            Just factors -> factors
            Nothing -> []
