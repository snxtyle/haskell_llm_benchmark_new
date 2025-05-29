module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (foldl')

isPalindrome :: Integer -> Bool
isPalindrome n = s == reverse s
  where s = show n

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = 
      let factorsPairs = [ (a, b) | a <- [minFactor..maxFactor], b <- [a..maxFactor] ]
          products = [ (p, a, b) | (a,b) <- factorsPairs, let p = a*b, isPalindrome p ]
      in case foldl' f Nothing products of
           Nothing -> Nothing
           Just (p, pairs) -> Just (p, pairs)
  where
    f Nothing (p, a, b) = Just (p, [(a,b)])
    f (Just (current, fs)) (p, a, b)
        | p > current = Just (p, [(a,b)])
        | p == current = Just (p, (a,b):fs)
        | otherwise = Just (current, fs)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = 
      let factorsPairs = [ (a, b) | a <- [minFactor..maxFactor], b <- [a..maxFactor] ]
          products = [ (p, a, b) | (a,b) <- factorsPairs, let p = a*b, isPalindrome p ]
      in case foldl' f Nothing products of
           Nothing -> Nothing
           Just (p, pairs) -> Just (p, pairs)
  where
    f Nothing (p, a, b) = Just (p, [(a,b)])
    f (Just (current, fs)) (p, a, b)
        | p < current = Just (p, [(a,b)])
        | p == current = Just (p, (a,b):fs)
        | otherwise = Just (current, fs)
