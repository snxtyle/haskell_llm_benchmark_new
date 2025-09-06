module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite xs = unlines (map verse (zip xs (drop 1 xs))) ++ lastVerse
  where
    verse (a, b) = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    lastVerse = "And all for the want of a " ++ head xs ++ "."
