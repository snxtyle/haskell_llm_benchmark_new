module Sublist (sublist) where

import Data.List (isPrefixOf, tails)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist [] [] = Just EQ
sublist [] _  = Just LT
sublist _  [] = Just GT
sublist xs ys
  | xs == ys = Just EQ
  | isSublist xs ys = Just LT
  | isSublist ys xs = Just GT
  | otherwise = Nothing

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist xs ys = any (xs `isPrefixOf`) (tails ys)
