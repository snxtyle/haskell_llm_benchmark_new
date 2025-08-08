module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go :: [Char] -> String -> Bool
    go stack [] = null stack
    go stack (c:cs)
      | c == '(' || c == '[' || c == '{' = go (c:stack) cs
      | c == ')' || c == ']' || c == '}' =
          case stack of
            (s:ss) | matches s c -> go ss cs
            _ -> False
      | otherwise = go stack cs

    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _     = False
