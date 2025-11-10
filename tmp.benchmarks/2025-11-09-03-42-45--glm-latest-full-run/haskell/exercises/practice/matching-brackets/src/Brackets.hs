module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go xs []
  where
    go :: String -> String -> Bool
    go [] [] = True
    go [] _  = False
    go (y:ys) stack
        | y `elem` "([{" = go ys (y:stack)
        | y `elem` ")]}" = case stack of
                             [] -> False
                             (s:ss) -> if isMatching s y then go ys ss else False
        | otherwise      = go ys stack

isMatching :: Char -> Char -> Bool
isMatching '(' ')' = True
isMatching '[' ']' = True
isMatching '{' '}' = True
isMatching _ _     = False
