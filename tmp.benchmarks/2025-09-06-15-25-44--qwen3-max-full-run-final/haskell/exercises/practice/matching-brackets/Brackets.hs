module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = checkBrackets [] (filter isBracket xs)
  where
    isBracket c = c `elem` "[]{}()"
    
    checkBrackets :: String -> String -> Bool
    checkBrackets stack [] = null stack
    checkBrackets stack (c:cs) = case c of
        '(' -> checkBrackets (')':stack) cs
        '[' -> checkBrackets (']':stack) cs
        '{' -> checkBrackets ('}':stack) cs
        ')' -> case stack of
            ')':rest -> checkBrackets rest cs
            _ -> False
        ']' -> case stack of
            ']':rest -> checkBrackets rest cs
            _ -> False
        '}' -> case stack of
            '}':rest -> checkBrackets rest cs
            _ -> False
        _ -> False
