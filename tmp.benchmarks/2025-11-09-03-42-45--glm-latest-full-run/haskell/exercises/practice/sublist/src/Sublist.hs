module Sublist (sublist) where

import Data.List (isInfixOf)

-- | Determines the relationship between two lists.
-- Returns:
--   Just EQ  if the lists are equal.
--   Just LT  if the first list is a sublist of the second.
--   Just GT  if the first list is a superlist of the second.
--   Nothing  if the lists are unequal.
sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys          = Just EQ
  | isInfixOf xs ys   = Just LT
  | isInfixOf ys xs   = Just GT
  | otherwise         = Nothing
