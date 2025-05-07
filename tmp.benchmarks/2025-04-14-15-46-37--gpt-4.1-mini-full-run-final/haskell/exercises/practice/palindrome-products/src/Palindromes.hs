module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all factor pairs (a,b) with minFactor <= a <= b <= maxFactor
factorPairs :: Integer -> Integer -> [(Integer, Integer)]
factorPairs minF maxF = [(a,b) | a <- [minF..maxF], b <- [a..maxF]]

-- Find all palindrome products with their factor pairs
palindromeProducts :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
palindromeProducts minF maxF =
  let pairs = factorPairs minF maxF
      products = [(p, (a,b)) | (a,b) <- pairs, let p = a*b, isPalindrome p]
      grouped = foldr insertProduct [] products
  in grouped
  where
    insertProduct (p, f) [] = [(p, [f])]
    insertProduct (p, f) ((p', fs):rest)
      | p == p'   = (p, f:fs) : rest
      | otherwise = (p', fs) : insertProduct (p, f) rest

-- Find the smallest palindrome product and its factors
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minF maxF =
  let pals = palindromeProducts minF maxF
  in if null pals then Nothing else Just $ minimumByFst pals

-- Find the largest palindrome product and its factors
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minF maxF =
  let pals = palindromeProducts minF maxF
  in if null pals then Nothing else Just $ maximumByFst pals

-- Helper functions to get minimum and maximum by fst element of tuple
minimumByFst :: Ord a => [(a, b)] -> (a, b)
minimumByFst = foldr1 (\x@(a,_) y@(b,_) -> if a <= b then x else y)

maximumByFst :: Ord a => [(a, b)] -> (a, b)
maximumByFst = foldr1 (\x@(a,_) y@(b,_) -> if a >= b then x else y)
