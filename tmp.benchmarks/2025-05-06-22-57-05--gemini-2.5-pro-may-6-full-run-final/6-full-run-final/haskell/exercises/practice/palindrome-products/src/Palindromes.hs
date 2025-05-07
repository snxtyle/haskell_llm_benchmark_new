module Palindromes (largestPalindrome, smallestPalindrome) where

-- No explicit imports are needed as all functions used (show, reverse, map, filter, maximum, minimum)
-- are part of the Prelude, which is included in `base`.

-- Helper function to check if a number is a palindrome.
-- Assumes n is non-negative, which is true for products of factors from typical positive ranges.
isPalindrome :: Integer -> Bool
isPalindrome n = s == reverse s
  where s = show n

-- Helper function to generate all palindrome products and their factor pairs
-- within the given range [minFactor, maxFactor].
-- Factor pairs (f1, f2) are stored such that f1 <= f2.
generatePalindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
generatePalindromeProducts minFactor maxFactor
  -- If minFactor > maxFactor, the list comprehension [minFactor..maxFactor]
  -- will be empty, resulting in an empty list of products, which is correct.
  -- Adding an explicit guard for clarity or robustness if desired:
  | minFactor > maxFactor = []
  | otherwise =
      let -- Generate products i*j with i <= j.
          -- This ensures each pair of factors is considered once (e.g., 2*3)
          -- and standardizes the order within the factor pair tuple (i.e., factor1 <= factor2).
          productsAndFactors = [(i*j, (i,j)) | i <- [minFactor..maxFactor],
                                                j <- [i..maxFactor]]
      -- Filter to keep only those products that are palindromes.
      in filter (\(productValue, _factors) -> isPalindrome productValue) productsAndFactors

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  -- Get all palindrome products along with their factor pairs.
  let palindromesWithFactors = generatePalindromeProducts minFactor maxFactor
  in case palindromesWithFactors of
       [] -> Nothing -- No palindrome products found in the range.
       _  ->
         -- Find the numerically largest palindrome value among them.
         let maxPalindromeValue = maximum $ map fst palindromesWithFactors
         -- Collect all factor pairs that result in this largest palindrome value.
         -- This handles cases where a palindrome has multiple pairs of factors.
             factorsForMaxPalindrome = [factors | (pValue, factors) <- palindromesWithFactors, pValue == maxPalindromeValue]
         in Just (maxPalindromeValue, factorsForMaxPalindrome)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  -- Get all palindrome products along with their factor pairs.
  let palindromesWithFactors = generatePalindromeProducts minFactor maxFactor
  in case palindromesWithFactors of
       [] -> Nothing -- No palindrome products found in the range.
       _  ->
         -- Find the numerically smallest palindrome value among them.
         let minPalindromeValue = minimum $ map fst palindromesWithFactors
         -- Collect all factor pairs that result in this smallest palindrome value.
             factorsForMinPalindrome = [factors | (pValue, factors) <- palindromesWithFactors, pValue == minPalindromeValue]
         in Just (minPalindromeValue, factorsForMinPalindrome)
