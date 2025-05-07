module Sublist (sublist) where

import Data.List (isInfixOf)

-- | Given two lists, determine if the first list is equal to, a sublist of,
-- | a superlist of, or none of these when compared to the second list.
-- |
-- | EQ: The lists are equal.
-- | LT: The first list is a sublist of the second.
-- | GT: The first list is a superlist of the second.
-- | Nothing: The lists are not equal, nor is one a sublist/superlist of the other.
sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
    | xs == ys         = Just EQ        -- A is equal to B
    | isInfixOf xs ys  = Just LT        -- A is a sublist of B
    | isInfixOf ys xs  = Just GT        -- A is a superlist of B (B is a sublist of A)
    | otherwise        = Nothing        -- A and B are unequal
