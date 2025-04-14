module Palindromes (largestPalindrome, smallestPalindrome) where

-- Helper function to check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing  -- Invalid range
  | otherwise =
      let candidates = [(i * j, (i, j)) | i <- [minFactor .. maxFactor],
                                         j <- [minFactor .. maxFactor],
                                         i <= j,  -- Ensure i <= j to avoid duplicates
                                         isPalindrome (i * j)]
      in if null candidates
         then Nothing
         else let maxP = maximum [p | (p, _) <- candidates]
                  factors = [pair | (p, pair) <- candidates, p == maxP]
              in Just (maxP, factors)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing  -- Invalid range
  | otherwise =
      let candidates = [(i * j, (i, j)) | i <- [minFactor .. maxFactor],
                                         j <- [minFactor .. maxFactor],
                                         i <= j,  -- Ensure i <= j to avoid duplicates
                                         isPalindrome (i * j)]
      in if null candidates
         then Nothing
         else let minP = minimum [p | (p, _) <- candidates]
                  factors = [pair | (p, pair) <- candidates, p == minP]
              in Just (minP, factors)
