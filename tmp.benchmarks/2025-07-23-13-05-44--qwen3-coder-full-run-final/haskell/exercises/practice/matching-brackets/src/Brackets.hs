module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = null . foldl process []
  where
    process stack c
      | c == '(' = '(' : stack
      | c == '[' = '[' : stack
      | c == '{' = '{' : stack
      | c == ')' && not (null stack) && head stack == '(' = tail stack
      | c == ']' && not (null stack) && head stack == '[' = tail stack
      | c == '}' && not (null stack) && head stack == '{' = tail stack
      | c `elem` ")]}" = c : stack
      | otherwise = stack
