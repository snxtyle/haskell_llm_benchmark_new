module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = check [] xs
  where
    check :: String -> String -> Bool
    check [] [] = True
    check _ [] = False
    check stack (c:cs)
      | c `elem` "([{" = check (c:stack) cs
      | c `elem` ")]}" = case stack of
          [] -> False
          (s:ss) -> if matches s c
                    then check ss cs
                    else False
      | otherwise = check stack cs
    
    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _ = False
