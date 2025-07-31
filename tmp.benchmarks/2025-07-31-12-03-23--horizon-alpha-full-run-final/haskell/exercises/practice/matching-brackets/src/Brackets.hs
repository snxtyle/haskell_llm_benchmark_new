module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go :: [Char] -> String -> Bool
    go stack [] = null stack
    go stack (c:cs)
      | isOpen c  = go (c : stack) cs
      | isClose c = case stack of
                      (o:rest) | matches o c -> go rest cs
                      _ -> False
      | otherwise = go stack cs

    isOpen :: Char -> Bool
    isOpen c = c == '(' || c == '[' || c == '{'

    isClose :: Char -> Bool
    isClose c = c == ')' || c == ']' || c == '}'

    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _   _   = False
