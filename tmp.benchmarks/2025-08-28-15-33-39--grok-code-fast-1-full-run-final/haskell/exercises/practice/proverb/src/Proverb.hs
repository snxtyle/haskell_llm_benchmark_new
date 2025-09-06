module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xs@(x:_) = intercalate "\n" (map makeLine pairs ++ [lastLine])
  where
    pairs = zip xs (tail xs)
    makeLine (a, b) = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    lastLine = "And all for the want of a " ++ x ++ "."
