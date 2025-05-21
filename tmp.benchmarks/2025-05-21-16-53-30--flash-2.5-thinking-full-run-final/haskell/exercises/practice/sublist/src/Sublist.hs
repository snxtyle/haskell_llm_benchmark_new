module Sublist (sublist) where

import Data.List (isPrefixOf, tails)

-- | Checks if 'needle' is a contiguous subsequence of 'haystack'.
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True -- An empty list is a subsequence of any list
isSubsequenceOf _ [] = False -- A non-empty list cannot be a subsequence of an empty list
isSubsequenceOf needle haystack = any (isPrefixOf needle) (tails haystack)

-- | Determines the relationship between two lists:
--   - Just EQ: if xs is equal to ys
--   - Just LT: if xs is a sublist of ys
--   - Just GT: if xs is a superlist of ys
--   - Nothing: if none of the above is true
sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | isSubsequenceOf xs ys = Just LT
  | isSubsequenceOf ys xs = Just GT
  | otherwise = Nothing
