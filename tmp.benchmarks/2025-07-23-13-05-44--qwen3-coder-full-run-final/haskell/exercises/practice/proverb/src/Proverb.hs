module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite xs = unlines (verseLines ++ [lastLine])
  where
    verseLines = zipWith verse xs (drop 1 xs)
    verse want lost = "For want of a " ++ want ++ " the " ++ lost ++ " was lost."
    lastLine = "And all for the want of a " ++ head xs ++ "."
