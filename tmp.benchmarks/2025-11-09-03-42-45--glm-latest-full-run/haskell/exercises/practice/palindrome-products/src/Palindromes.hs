module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort, groupBy)
import Data.Function (on)

-- | Checks if a number is a palindrome.
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- | Finds all palindrome products within a given factor range, along with their factors.
-- The result is a list of tuples, where each tuple contains the palindrome product
-- and a list of all factor pairs that produce it. The list is sorted by the
-- palindrome product.
findPalindromes :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
findPalindromes minFactor maxFactor
  | minFactor > maxFactor = []
  | otherwise =
      let -- Generate all products and their factor pairs.
          -- We use b <- [a..maxFactor] to avoid duplicate factor pairs like (3,5) and (5,3).
          products = [(a * b, (a, b)) | a <- [minFactor..maxFactor], b <- [a..maxFactor]]
          
          -- Filter for products that are palindromes.
          palindromes = filter (isPalindrome . fst) products
          
          -- Sort the palindromes by their value (the first element of the tuple).
          sortedPalindromes = sort palindromes
          
          -- Group the sorted list by the palindrome value.
          -- The result is a list of groups, where each group is a list of
          -- (palindrome, (factor1, factor2)) tuples with the same palindrome.
          grouped = groupBy ((==) `on` fst) sortedPalindromes
          
          -- Transform the grouped data into the desired format:
          -- (palindrome, [list of factor pairs]).
          formatGroup group = (fst (head group), map snd group)
          
      in map formatGroup grouped

-- | Finds the smallest palindrome product for a given range of factors.
-- Returns Nothing if no palindrome is found.
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let palindromes = findPalindromes minFactor maxFactor
  in if null palindromes then Nothing else Just (head palindromes)

-- | Finds the largest palindrome product for a given range of factors.
-- Returns Nothing if no palindrome is found.
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let palindromes = findPalindromes minFactor maxFactor
  in if null palindromes then Nothing else Just (last palindromes)
