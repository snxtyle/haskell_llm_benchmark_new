module Palindromes (largestPalindrome, smallestPalindrome) where

-- Helper function to check if a number is a palindrome
-- It converts the number to a string and checks if the string equals its reverse.
-- It also handles negative numbers (which are not considered palindromes here).
isPalindrome :: Integer -> Bool
isPalindrome n = n >= 0 && show n == reverse (show n)

-- Helper function to generate all palindrome products and their factors within the range.
-- Returns a list of tuples: (palindromeProduct, (factor1, factor2))
findPalindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
findPalindromeProducts minFactor maxFactor
  -- If minFactor is greater than maxFactor, the range is invalid, return empty list.
  | minFactor > maxFactor = []
  | otherwise =
      [ (p, (i, j)) -- Include the product and its factors
      | i <- [minFactor..maxFactor] -- Iterate through the first factor
      , j <- [minFactor..maxFactor] -- Iterate through the second factor (allow i*j and j*i)
      , let p = i * j             -- Calculate the product
      , isPalindrome p            -- Check if the product is a palindrome
      ]

-- Finds the largest palindrome product within the given factor range.
-- Returns Nothing if no palindrome product exists.
-- Otherwise, returns Just (palindrome, [(factor1, factor2), ...])
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let palindromes = findPalindromeProducts minFactor maxFactor -- Get all palindrome products and factors
  in case palindromes of
       [] -> Nothing -- No palindromes found
       _  -> let maxP = maximum (map fst palindromes) -- Find the largest palindrome value
                 -- Filter the original list to get all pairs of factors for the largest palindrome
                 factors = [fs | (p, fs) <- palindromes, p == maxP]
             in Just (maxP, factors) -- Return the largest palindrome and its factors

-- Finds the smallest palindrome product within the given factor range.
-- Returns Nothing if no palindrome product exists.
-- Otherwise, returns Just (palindrome, [(factor1, factor2), ...])
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let palindromes = findPalindromeProducts minFactor maxFactor -- Get all palindrome products and factors
  in case palindromes of
       [] -> Nothing -- No palindromes found
       _  -> let minP = minimum (map fst palindromes) -- Find the smallest palindrome value
                 -- Filter the original list to get all pairs of factors for the smallest palindrome
                 factors = [fs | (p, fs) <- palindromes, p == minP]
             in Just (minP, factors) -- Return the smallest palindrome and its factors
