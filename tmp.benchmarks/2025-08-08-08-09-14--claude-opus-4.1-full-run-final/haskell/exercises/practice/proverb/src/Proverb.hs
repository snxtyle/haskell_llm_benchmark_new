module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [single] = "And all for the want of a " ++ single ++ "."
recite items@(first:_) = unlines (wantLines ++ [lastLine])
  where
    wantLines = zipWith makeLine items (drop 1 items)
    makeLine x y = "For want of a " ++ x ++ " the " ++ y ++ " was lost."
    lastLine = "And all for the want of a " ++ first ++ "."
