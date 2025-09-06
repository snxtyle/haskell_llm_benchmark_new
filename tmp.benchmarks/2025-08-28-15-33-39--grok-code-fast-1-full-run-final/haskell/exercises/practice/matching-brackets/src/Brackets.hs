module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go xs []
  where
    go [] stack = null stack
    go (c:cs) stack
      | c `elem` "([{" = go cs (c:stack)
      | c `elem` ")]}" =
          case stack of
            [] -> False
            (top:rest) -> if matches top c then go cs rest else False
      | otherwise = go cs stack

    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _ = False
