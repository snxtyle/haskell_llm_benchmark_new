module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite items = unlines (verseLines items ++ [finalLine items])
  where
    verseLines :: [String] -> [String]
    verseLines [] = []
    verseLines [_] = []
    verseLines (x:y:xs) = ("For want of a " ++ x ++ " the " ++ y ++ " was lost.") : verseLines (y:xs)
    
    finalLine :: [String] -> String
    finalLine (x:_) = "And all for the want of a " ++ x ++ "."
