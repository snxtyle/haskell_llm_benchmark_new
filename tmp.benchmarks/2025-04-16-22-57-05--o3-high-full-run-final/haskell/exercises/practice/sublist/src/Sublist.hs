module Sublist (sublist) where

import Data.List (isPrefixOf, tails)

-- | Determine the relationship between two lists.
--   Returns:
--     * Just EQ – lists are equal
--     * Just LT – first list is a (contiguous) sublist of the second
--     * Just GT – first list is a superlist of the second
--     * Nothing – none of the above
sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys                 = Just EQ
  | xs `isSublistOf` ys      = Just LT
  | ys `isSublistOf` xs      = Just GT
  | otherwise                = Nothing
  where
    -- True when the first list occurs as a contiguous slice within the second.
    isSublistOf :: Eq a => [a] -> [a] -> Bool
    isSublistOf small big = any (small `isPrefixOf`) (tails big)
