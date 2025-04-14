module Palindromes (largestPalindrome, smallestPalindrome) where

-- Helper function to check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- Helper function to generate all factor pairs within range
getFactorPairs :: Integer -> Integer -> Integer -> [(Integer, Integer)]
getFactorPairs minFactor maxFactor product =
    [(x, y) | x <- [minFactor..maxFactor], 
              y <- [x..maxFactor], 
              x * y == product]

-- Helper function to get all products and their factors
getProductsWithFactors :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
getProductsWithFactors minFactor maxFactor =
    let products = [x * y | x <- [minFactor..maxFactor], y <- [x..maxFactor]]
        palindromes = filter isPalindrome products
    in map (\p -> (p, getFactorPairs minFactor maxFactor p)) palindromes

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise =
        let products = getProductsWithFactors minFactor maxFactor
        in case products of
            [] -> Nothing
            _  -> Just $ maximum products

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | minFactor > maxFactor = Nothing
    | otherwise =
        let products = getProductsWithFactors minFactor maxFactor
        in case products of
            [] -> Nothing
            _  -> Just $ minimum products
