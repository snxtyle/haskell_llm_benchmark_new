module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = checkBrackets xs []
  where
    checkBrackets :: String -> [Char] -> Bool
    checkBrackets [] stack = null stack
    checkBrackets (c:cs) stack
      | c `elem` "([{" = checkBrackets cs (c:stack)
      | c `elem` ")]}" = case stack of
          []     -> False
          (s:ss) -> isMatchingPair s c && checkBrackets cs ss
      | otherwise = checkBrackets cs stack
    
    isMatchingPair :: Char -> Char -> Bool
    isMatchingPair '(' ')' = True
    isMatchingPair '[' ']' = True
    isMatchingPair '{' '}' = True
    isMatchingPair _ _ = False
