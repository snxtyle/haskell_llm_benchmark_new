module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- Find all factor pairs of a number within the given range
findFactors :: Integer -> Integer -> Integer -> [(Integer, Integer)]
findFactors minFactor maxFactor n = 
    [(i, j) | i <- [minFactor..maxFactor], 
              j <- [i..maxFactor], 
              i * j == n]

-- Generate all palindrome products within the range
palindromeProducts :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
palindromeProducts minFactor maxFactor = 
    [(prod, factors) | i <- [minFactor..maxFactor],
                       j <- [i..maxFactor],
                       let prod = i * j,
                       isPalindrome prod,
                       let factors = findFactors minFactor maxFactor prod,
                       not (null factors)]

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | null palindromes = Nothing
    | otherwise = Just (maxPalindrome, maxFactors)
    where palindromes = palindromeProducts minFactor maxFactor
          (maxPalindrome, maxFactors) = maximumBy (\(a, _) (b, _) -> compare a b) palindromes
          maximumBy _ [] = error "empty list"
          maximumBy f (x:xs) = foldl (\acc y -> if f acc y == LT then y else acc) x xs

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | null palindromes = Nothing
    | otherwise = Just (minPalindrome, minFactors)
    where palindromes = palindromeProducts minFactor maxFactor
          (minPalindrome, minFactors) = minimumBy (\(a, _) (b, _) -> compare a b) palindromes
          minimumBy _ [] = error "empty list"
          minimumBy f (x:xs) = foldl (\acc y -> if f acc y == GT then y else acc) x xs
