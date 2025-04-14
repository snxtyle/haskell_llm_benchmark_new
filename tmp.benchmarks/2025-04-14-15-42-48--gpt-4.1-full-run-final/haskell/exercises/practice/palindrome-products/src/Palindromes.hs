module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (nub, sort, groupBy)
import Data.Maybe (listToMaybe)
import Data.Function (on)

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all factor pairs (a, b) such that minF <= a <= b <= maxF
factorPairs :: Integer -> Integer -> [(Integer, Integer)]
factorPairs minF maxF =
  [ (a, b)
  | a <- [minF..maxF]
  , b <- [a..maxF]
  ]

-- For a given range, get all palindrome products and their factor pairs
palindromeProductsWithFactors :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProductsWithFactors minF maxF =
  [ (p, (a, b))
  | (a, b) <- factorPairs minF maxF
  , let p = a * b
  , isPalindrome p
  ]

-- Group factor pairs by their palindrome product
groupedPalindromes :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
groupedPalindromes minF maxF =
  let palProds = palindromeProductsWithFactors minF maxF
      grouped = groupBy ((==) `on` fst) $ sort palProds
  in [ (fst (head g), map snd g) | g <- grouped ]

-- Find the smallest palindrome and its factor pairs
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minF maxF =
  let palProds = groupedPalindromes minF maxF
  in case palProds of
       [] -> Nothing
       _  -> Just $ head palProds

-- Find the largest palindrome and its factor pairs
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minF maxF =
  let palProds = groupedPalindromes minF maxF
  in case palProds of
       [] -> Nothing
       _  -> Just $ last palProds
