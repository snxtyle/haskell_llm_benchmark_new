module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys              = Just EQ
  | xs `isInfixOf` ys     = Just LT
  | ys `isInfixOf` xs     = Just GT
  | otherwise             = Nothing

-- Checks whether the first list occurs as a contiguous subsequence of the second.
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack
  | null needle           = True
  | null haystack         = False
  | needle `isPrefixOf` haystack = True
  | otherwise             = isInfixOf needle (tail haystack)

-- Checks whether the first list is a prefix of the second.
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
