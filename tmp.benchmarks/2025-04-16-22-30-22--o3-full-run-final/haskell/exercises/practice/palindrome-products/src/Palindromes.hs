module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (sort)

-- | Check whether a given integer is a palindrome.
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

-- | Generate all palindromic products together with one representative
--   factor pair.  We only consider pairs with a ≤ b so that each product
--   appears at most once.
palindromes :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromes minF maxF =
  [ (p, (a, b))
  | a <- [minF .. maxF]
  , b <- [a    .. maxF]  -- ensure a ≤ b to avoid mirrored duplicates
  , let p = a * b
  , isPalindrome p
  ]

-- | Helper that selects either the minimum or maximum palindrome,
--   then gathers all factor pairs that yield that palindrome.
selectPalindrome
  :: ([Integer] -> Integer)          -- ^ minimum or maximum
  -> Integer                         -- ^ lower factor bound
  -> Integer                         -- ^ upper factor bound
  -> Maybe (Integer, [(Integer, Integer)])
selectPalindrome pick minF maxF
  | minF > maxF = Nothing            -- invalid range
  | null ps     = Nothing            -- no palindromes in range
  | otherwise   = Just (target, sort factors)
  where
    ps       = palindromes minF maxF
    target   = pick (map fst ps)
    factors  = map snd (filter ((== target) . fst) ps)

-- | Smallest palindromic product in the given factor range.
smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome = selectPalindrome minimum

-- | Largest palindromic product in the given factor range.
largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome = selectPalindrome maximum
