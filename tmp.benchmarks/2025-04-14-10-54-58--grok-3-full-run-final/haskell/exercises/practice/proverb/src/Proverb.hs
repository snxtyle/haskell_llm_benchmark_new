module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite (x:xs) = unlines (map line (zip (x:xs) (xs ++ [x]))) ++ finalLine x
  where
    line (a, b) = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    finalLine item = "And all for the want of a " ++ item ++ "."
