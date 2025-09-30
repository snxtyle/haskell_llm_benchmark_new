module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite xs = intercalate "\n" (lines ++ [lastLine])
  where
    lines = zipWith makeLine xs (tail xs)
    makeLine item1 item2 = "For want of a " ++ item1 ++ " the " ++ item2 ++ " was lost."
    lastLine = "And all for the want of a " ++ head xs ++ "."
