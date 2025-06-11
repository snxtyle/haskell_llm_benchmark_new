module Palindromes
  ( largestPalindrome
  , smallestPalindrome
  ) where

import Data.List (foldl')

-- | Determine whether an integer is a palindrome.
--   A negative number is never considered a palindrome here,
--   because of the leading minus sign.
isPalindrome :: Integer -> Bool
isPalindrome n
  | n < 0     = False
  | otherwise = let s = show n in s == reverse s

-- | Produce all factor pairs (a, b) with a <= b that lie within the given limits.
factorPairs :: Integer -> Integer -> [(Integer, Integer)]
factorPairs minFactor maxFactor =
  [ (a, b)
  | a <- [minFactor .. maxFactor]
  , b <- [a         .. maxFactor]
  ]

-- | Helper that, given the list of palindromic products, calculates the
--   requested extremum (min or max).
selectPalindrome
  :: (Integer -> Integer -> Bool)          -- ^ Comparator (choose min or max)
  -> Integer                               -- ^ Lower factor limit
  -> Integer                               -- ^ Upper factor limit
  -> Maybe (Integer, [(Integer, Integer)]) -- ^ Result
selectPalindrome cmp minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | null palindromic      = Nothing
  | otherwise             = Just (bestVal, bestPairs)
  where
    pairsWithProduct = [ (a * b, (a, b)) | (a, b) <- factorPairs minFactor maxFactor ]
    palindromic      = filter (isPalindrome . fst) pairsWithProduct

    -- Use a strict left fold to avoid building large intermediate lists
    (bestVal, bestPairs) =
      foldl' step (initialVal, []) palindromic

    initialVal = fst (head palindromic)

    step (currentBest, acc) (val, pair)
      | val `cmp` currentBest = (val, [pair])      -- Found a better extremum
      | val == currentBest   = (currentBest, pair : acc) -- Same extremum; accumulate
      | otherwise            = (currentBest, acc)

-- | Find the largest palindromic product within the factor range, and all its
--   factor pairs.
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome = selectPalindrome (>)

-- | Find the smallest palindromic product within the factor range, and all its
--   factor pairs.
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome = selectPalindrome (<)
