module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if an integer is a palindrome by comparing its string representation
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all palindromic products and their factor pairs (a <= b) within [lo..hi]
palindromicProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromicProducts lo hi
  | lo > hi = []
  | otherwise =
      [ (p, (a, b))
      | a <- [lo .. hi]
      , b <- [a .. hi]
      , let p = a * b
      , isPalindrome p
      ]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise =
      case palindromicProducts minFactor maxFactor of
        [] -> Nothing
        xs ->
          let maxVal = maximum (map fst xs)
              pairs  = [ pair | (p, pair) <- xs, p == maxVal ]
          in Just (maxVal, pairs)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise =
      case palindromicProducts minFactor maxFactor of
        [] -> Nothing
        xs ->
          let minVal = minimum (map fst xs)
              pairs  = [ pair | (p, pair) <- xs, p == minVal ]
          in Just (minVal, pairs)
