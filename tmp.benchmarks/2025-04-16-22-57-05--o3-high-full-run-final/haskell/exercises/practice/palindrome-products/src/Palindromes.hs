module Palindromes
  ( largestPalindrome
  , smallestPalindrome
  ) where

import Data.List (maximumBy, minimumBy)
import Data.Ord  (comparing)

-- | Determine whether a given integer is a palindrome.
--   Negative numbers are considered non‑palindromic.
isPalindrome :: Integer -> Bool
isPalindrome n
  | n < 0     = False
  | otherwise = let s = show n in s == reverse s

-- | Generate every factor pair (a,b) with a <= b that lies inside the
--   inclusive range [minFactor .. maxFactor].
factorPairs :: Integer -> Integer -> [(Integer, Integer)]
factorPairs minFactor maxFactor =
  [ (a, b)
  | a <- [minFactor .. maxFactor]
  , b <- [a         .. maxFactor]
  ]

-- | Produce all palindromic products together with the factor pair that
--   generated each product.
palindromeProducts
  :: Integer                    -- ^ lower factor bound (inclusive)
  -> Integer                    -- ^ upper factor bound (inclusive)
  -> [(Integer, (Integer, Integer))] -- ^ (product, (factor1,factor2))
palindromeProducts minFactor maxFactor =
  [ (p, pair)
  | pair@(a, b) <- factorPairs minFactor maxFactor
  , let p = a * b
  , isPalindrome p
  ]

-- | Helper that, given a selector (minimum or maximum), extracts the required
--   palindrome product and all of its factor pairs.
selectPalindrome
  :: ([(Integer, (Integer, Integer))] -> (Integer, (Integer, Integer))) -- ^ selector (min or max)
  -> Integer                                                             -- ^ lower bound
  -> Integer                                                             -- ^ upper bound
  -> Maybe (Integer, [(Integer, Integer)])                               -- ^ result
selectPalindrome selector minF maxF
  | minF > maxF            = Nothing
  | null palProducts       = Nothing
  | otherwise              = Just (palValue, factorsForValue)
  where
    palProducts       = palindromeProducts minF maxF
    (palValue, _)     = selector palProducts
    factorsForValue   =
      [ pair
      | (v, pair) <- palProducts
      , v == palValue
      ]

-- | Find the largest palindromic product within the factor limits.
largestPalindrome
  :: Integer                       -- ^ lower factor bound (inclusive)
  -> Integer                       -- ^ upper factor bound (inclusive)
  -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome =
  selectPalindrome (maximumBy (comparing fst))

-- | Find the smallest palindromic product within the factor limits.
smallestPalindrome
  :: Integer                       -- ^ lower factor bound (inclusive)
  -> Integer                       -- ^ upper factor bound (inclusive)
  -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome =
  selectPalindrome (minimumBy (comparing fst))
