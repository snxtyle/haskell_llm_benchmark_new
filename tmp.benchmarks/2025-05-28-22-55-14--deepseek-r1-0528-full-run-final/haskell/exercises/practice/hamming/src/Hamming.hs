module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = go xs ys 0
  where
    go [] [] n = Just n
    go (x:xs) (y:ys) n = 
        let n' = if x == y then n else n + 1
        in n' `seq` go xs ys n'
    go _ _ _ = Nothing
