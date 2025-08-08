module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xs = intercalate "\n" (chainLines ++ [finalLine])
  where
    chainLines = [ "For want of a " ++ a ++ " the " ++ b ++ " was lost."
                 | (a, b) <- zip xs (tail xs)
                 ]
    finalLine = "And all for the want of a " ++ head xs ++ "."
