module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Find all factor pairs for a given palindrome within the range
findFactorsForPalindrome :: Integer -> Integer -> Integer -> [(Integer, Integer)]
findFactorsForPalindrome minFactor maxFactor palindrome =
  [(i, j) | i <- [minFactor..maxFactor],
            let (j, remainder) = palindrome `divMod` i,
            remainder == 0,
            j >= i,
            j <= maxFactor]

-- Find the largest palindrome by checking from the maximum possible product down
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = searchLargest (maxFactor * maxFactor)
  where
    minProduct = minFactor * minFactor
    searchLargest candidate
      | candidate < minProduct = Nothing
      | isPalindrome candidate =
          let factors = findFactorsForPalindrome minFactor maxFactor candidate
          in if null factors
             then searchLargest (candidate - 1)
             else Just (candidate, factors)
      | otherwise = searchLargest (candidate - 1)

-- Find the smallest palindrome by checking from the minimum possible product up
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = searchSmallest (minFactor * minFactor)
  where
    maxProduct = maxFactor * maxFactor
    searchSmallest candidate
      | candidate > maxProduct = Nothing
      | isPalindrome candidate =
          let factors = findFactorsForPalindrome minFactor maxFactor candidate
          in if null factors
             then searchSmallest (candidate + 1)
             else Just (candidate, factors)
      | otherwise = searchSmallest (candidate + 1)
