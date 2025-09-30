module Sublist (sublist) where

import Data.List (isInfixOf, tails)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | isSublistOf xs ys = Just LT
  | isSublistOf ys xs = Just GT
  | otherwise = Nothing

-- Check if the first list is a contiguous subsequence of the second
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf xs ys = xs `isInfixOf` ys
