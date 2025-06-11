module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort, groupBy)
import Data.Function (on)

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- This helper function finds all palindrome products and their factors.
-- The result is a list of (palindrome, [(factor1, factor2), ...]),
-- sorted by the palindrome value.
findPalindromes :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
findPalindromes minFactor maxFactor
  | minFactor > maxFactor = []
  | otherwise =
      let -- Generate products with factors.
          -- We use y <- [x..maxFactor] to avoid duplicate pairs of factors like (a,b) and (b,a).
          products = [(x * y, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor]]

          -- Filter for products that are palindromes.
          palindromesWithFactors = filter (isPalindrome . fst) products

          -- Sort by the palindrome value.
          sortedPalindromes = sort palindromesWithFactors

          -- Group by palindrome value to collect all factor pairs for each palindrome.
          grouped = groupBy ((==) `on` fst) sortedPalindromes

          -- Format the groups into the desired output structure.
      in map (\g -> (fst (head g), map snd g)) grouped

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  case findPalindromes minFactor maxFactor of
    [] -> Nothing
    ps -> Just (last ps)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  case findPalindromes minFactor maxFactor of
    [] -> Nothing
    ps -> Just (head ps)
