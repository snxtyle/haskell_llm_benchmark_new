module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite items = unlines (mainLines ++ [finalLine])
  where
    pairs = zip items (tail items)
    mainLines = [ "For want of a " ++ a ++ " the " ++ b ++ " was lost." | (a, b) <- pairs ]
    finalLine = "And all for the want of a " ++ head items ++ "."
