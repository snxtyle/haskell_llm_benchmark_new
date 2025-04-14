module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go []
  where
    go [] [] = True
    go _  [] = False
    go stack (x:xs)
      | x `elem` "({[" = go (x:stack) xs
      | x `elem` ")}]" = case stack of
          (s:rest) | matches s x -> go rest xs
          _ -> False
      | otherwise = go stack xs

    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _ = False
