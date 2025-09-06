module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = check xs []
  where
    check [] stack = null stack
    check (x:xs) stack
      | x `elem` "([{" = check xs (x:stack)
      | x == ')' = match '(' xs stack
      | x == ']' = match '[' xs stack
      | x == '}' = match '{' xs stack
      | otherwise = check xs stack
    
    match expectedChar xs stack = case stack of
      (top:rest) | top == expectedChar -> check xs rest
      _ -> False
