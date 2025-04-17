module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if a number is a palindrome by comparing its string to its reverse
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Generate all palindrome products in the given range, along with their factor pairs
palindromeProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromeProducts minFactor maxFactor =
  [ (product, (i, j))
  | i <- [minFactor .. maxFactor]
  , j <- [i .. maxFactor]
  , let product = i * j
  , isPalindrome product
  ]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let pals = palindromeProducts minFactor maxFactor
  in if null pals
     then Nothing
     else
       let maxVal = maximum (map fst pals)
           factors = [ pair | (val, pair) <- pals, val == maxVal ]
       in Just (maxVal, factors)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let pals = palindromeProducts minFactor maxFactor
  in if null pals
     then Nothing
     else
       let minVal = minimum (map fst pals)
           factors = [ pair | (val, pair) <- pals, val == minVal ]
       in Just (minVal, factors)
