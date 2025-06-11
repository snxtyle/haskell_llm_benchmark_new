module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite inputs@(x:_) = intercalate "\n" (verses ++ [lastLine])
  where
    verses = zipWith verseFor inputs (tail inputs)
    verseFor a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    lastLine = "And all for the want of a " ++ x ++ "."
