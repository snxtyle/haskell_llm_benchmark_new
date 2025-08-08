module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go stack [] = null stack
    go stack (c:cs)
      | c `elem` "([{" = go (c : stack) cs
      | c `elem` ")]}" =
          case stack of
            (s:ss) | matches s c -> go ss cs
            _ -> False
      | otherwise = go stack cs

    matches '(' c = c == ')'
    matches '[' c = c == ']'
    matches '{' c = c == '}'
    matches _   _ = False
