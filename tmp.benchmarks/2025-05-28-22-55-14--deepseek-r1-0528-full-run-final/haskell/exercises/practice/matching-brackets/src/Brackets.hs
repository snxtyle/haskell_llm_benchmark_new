module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go stack [] = null stack
    go stack (x:xs)
      | x `elem` "([{" = go (x:stack) xs
      | x == ')' = check '(' stack xs
      | x == ']' = check '[' stack xs
      | x == '}' = check '{' stack xs
      | otherwise = go stack xs

    check _ [] _ = False
    check b (c:cs) xs
      | c == b    = go cs xs
      | otherwise = False
