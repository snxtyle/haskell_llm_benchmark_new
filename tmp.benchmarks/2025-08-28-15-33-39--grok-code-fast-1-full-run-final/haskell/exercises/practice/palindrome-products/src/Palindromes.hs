module Palindromes (largestPalindrome, smallestPalindrome) where

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = let candidates = [ (p, a, b) | a <- [minFactor..maxFactor], b <- [a..maxFactor], let p = a * b, isPalindrome p ]
                in if null candidates
                   then Nothing
                   else let maxP = maximum (map (\(p, _, _) -> p) candidates)
                        in Just (maxP, [ (a, b) | (p, a, b) <- candidates, p == maxP ])

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = let candidates = [ (p, a, b) | a <- [minFactor..maxFactor], b <- [a..maxFactor], let p = a * b, isPalindrome p ]
                in if null candidates
                   then Nothing
                   else let minP = minimum (map (\(p, _, _) -> p) candidates)
                        in Just (minP, [ (a, b) | (p, a, b) <- candidates, p == minP ])
