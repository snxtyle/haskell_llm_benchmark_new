module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = arePairedHelper xs []

arePairedHelper :: String -> [Char] -> Bool
arePairedHelper [] stack = null stack
arePairedHelper (x:xs) stack
  | x `elem` "([{" = arePairedHelper xs (x:stack)
  | x `elem` ")]}" = case stack of
      [] -> False
      (top:rest) -> if matches top x
                    then arePairedHelper xs rest
                    else False
  | otherwise = arePairedHelper xs stack

matches :: Char -> Char -> Bool
matches '(' ')' = True
matches '[' ']' = True
matches '{' '}' = True
matches _ _ = False
