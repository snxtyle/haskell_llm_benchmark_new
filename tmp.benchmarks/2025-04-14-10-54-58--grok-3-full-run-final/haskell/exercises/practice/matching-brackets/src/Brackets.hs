module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = checkBrackets (filter isBracket xs) []
  where
    isBracket c = c `elem` "[]{}()"
    checkBrackets [] stack = null stack
    checkBrackets (x:xs) stack
      | isOpening x = checkBrackets xs (x:stack)
      | null stack = False
      | matches (head stack) x = checkBrackets xs (tail stack)
      | otherwise = False
    isOpening c = c `elem` "[{("
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _ = False
