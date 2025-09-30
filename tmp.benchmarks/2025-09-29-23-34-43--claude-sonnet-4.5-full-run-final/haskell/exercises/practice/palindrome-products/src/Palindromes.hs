module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = s == reverse s
  where s = show n

-- Find all factor pairs (i, j) where i <= j and minFactor <= i, j <= maxFactor
factorPairs :: Integer -> Integer -> Integer -> [(Integer, Integer)]
factorPairs minFactor maxFactor product = 
  [(i, j) | i <- [minFactor..maxFactor], 
            let j = product `div` i,
            product `mod` i == 0,
            j >= i,
            j >= minFactor,
            j <= maxFactor]

-- Find the largest palindrome product in the range
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = findLargest (maxFactor * maxFactor)
  where
    findLargest n
      | n < minFactor * minFactor = Nothing
      | isPalindrome n = 
          let factors = factorPairs minFactor maxFactor n
          in if null factors then findLargest (n - 1) else Just (n, factors)
      | otherwise = findLargest (n - 1)

-- Find the smallest palindrome product in the range
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = findSmallest (minFactor * minFactor)
  where
    findSmallest n
      | n > maxFactor * maxFactor = Nothing
      | isPalindrome n = 
          let factors = factorPairs minFactor maxFactor n
          in if null factors then findSmallest (n + 1) else Just (n, factors)
      | otherwise = findSmallest (n + 1)
