module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | isSubsequence xs ys = Just LT
  | isSubsequence ys xs = Just GT
  | otherwise = Nothing
  where
    -- Check if the first list is a contiguous subsequence of the second
    isSubsequence [] _ = True
    isSubsequence _ [] = False
    isSubsequence sub lst@(_:rest)
      | sub `isPrefixOf` lst = True
      | otherwise = isSubsequence sub rest
    
    -- Check if the first list is a prefix of the second
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs') (y:ys')
      | x == y = isPrefixOf xs' ys'
      | otherwise = False
