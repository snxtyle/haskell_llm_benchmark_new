module Sublist (sublist) where

import Data.List (isPrefixOf)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | isSublist xs ys = Just LT
  | isSublist ys xs = Just GT
  | otherwise = Nothing
  where
    isSublist [] _ = True
    isSublist _ [] = False
    isSublist sub list@(h:t)
      | sub `isPrefixOf` list = True
      | otherwise = isSublist sub t
