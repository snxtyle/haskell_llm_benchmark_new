module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite items = intercalate "\n" (proverbLines ++ [finalLine])
  where
    pairs = zip items (tail items)
    proverbLines = map makeLine pairs
    makeLine (want, lost) = "For want of a " ++ want ++ " the " ++ lost ++ " was lost."
    finalLine = "And all for the want of a " ++ head items ++ "."
