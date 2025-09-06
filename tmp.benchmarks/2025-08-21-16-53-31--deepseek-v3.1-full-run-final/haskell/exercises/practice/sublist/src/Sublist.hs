module Sublist (sublist) where

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
isSublist xs ys = any (isPrefixOf xs) (tails ys)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys
