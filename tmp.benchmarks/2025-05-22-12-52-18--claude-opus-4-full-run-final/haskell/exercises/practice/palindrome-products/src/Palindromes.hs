module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all products and their factor pairs within the given range
generateProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
generateProducts minFactor maxFactor
  | minFactor > maxFactor = []
  | otherwise = [(i * j, (i, j)) | i <- [minFactor..maxFactor], j <- [i..maxFactor]]

-- Find all factor pairs for a given palindrome within the range
findFactorPairs :: Integer -> Integer -> Integer -> [(Integer, Integer)]
findFactorPairs palindrome minFactor maxFactor =
  [(i, j) | i <- [minFactor..maxFactor], 
            j <- [i..maxFactor], 
            i * j == palindrome]

-- Find the largest palindrome product in the given range
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null palindromes = Nothing
  | otherwise = Just (largestPal, findFactorPairs largestPal minFactor maxFactor)
  where
    products = map fst $ generateProducts minFactor maxFactor
    palindromes = filter isPalindrome products
    largestPal = maximum palindromes

-- Find the smallest palindrome product in the given range
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null palindromes = Nothing
  | otherwise = Just (smallestPal, findFactorPairs smallestPal minFactor maxFactor)
  where
    products = map fst $ generateProducts minFactor maxFactor
    palindromes = filter isPalindrome products
    smallestPal = minimum palindromes
