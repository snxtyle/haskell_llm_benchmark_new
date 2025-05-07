module Proverb (recite) where

recite :: [String] -> String
recite [] = ""
recite [a] = "And all for the want of a " ++ a ++ "."
recite items = unlines (zipWith line items (tail items)) ++ finalLine (head items)
  where
    line a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    finalLine a = "And all for the want of a " ++ a ++ "."
