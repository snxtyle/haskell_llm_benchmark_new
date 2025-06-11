module Sublist (sublist) where

import Data.List (isInfixOf)

-- | Determine the sub-list relationship between two lists.
--   The result follows the Ordering convention:
--     Just LT → first list is a sublist  of the second
--     Just EQ → lists are equal
--     Just GT → first list is a superlist of the second
--     Nothing → none of the above (lists are unequal)
sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys          = Just EQ
  | xs `isInfixOf` ys = Just LT
  | ys `isInfixOf` xs = Just GT
  | otherwise         = Nothing
