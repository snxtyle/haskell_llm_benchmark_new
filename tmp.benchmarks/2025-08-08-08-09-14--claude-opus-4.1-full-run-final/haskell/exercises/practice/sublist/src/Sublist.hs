module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | xs `isSubsequenceOf` ys = Just LT
  | ys `isSubsequenceOf` xs = Just GT
  | otherwise = Nothing
  where
    -- Check if the first list appears as a contiguous subsequence in the second list
    isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
    isSubsequenceOf [] _ = True
    isSubsequenceOf _ [] = False
    isSubsequenceOf needle haystack@(_:rest)
      | needle `isPrefixOf` haystack = True
      | otherwise = needle `isSubsequenceOf` rest
    
    -- Check if the first list is a prefix of the second list
    isPrefixOf :: Eq a => [a] -> [a] -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && xs `isPrefixOf` ys
