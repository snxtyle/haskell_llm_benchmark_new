module Sublist (sublist) where

import Data.List (isInfixOf)

-- | Determine the relationship between two lists.
--   Returns:
--     Just EQ : the lists are equal
--     Just LT : the first list is a sublist  of the second
--     Just GT : the first list is a superlist of the second
--     Nothing : none of the above (lists are unequal)
sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys           = Just EQ
  | xs `isInfixOf` ys  = Just LT
  | ys `isInfixOf` xs  = Just GT
  | otherwise          = Nothing
