module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
    | xs == ys = Just EQ
    | isSublist xs ys = Just LT
    | isSublist ys xs = Just GT
    | otherwise = Nothing
  where
    isSublist [] _ = True
    isSublist _ [] = False
    isSublist xs@(x:xt) (y:yt) =
        if x == y
            then isPrefix xs (y:yt) || isSublist xs yt
            else isSublist xs yt
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xt) (y:yt) = x == y && isPrefix xt yt
