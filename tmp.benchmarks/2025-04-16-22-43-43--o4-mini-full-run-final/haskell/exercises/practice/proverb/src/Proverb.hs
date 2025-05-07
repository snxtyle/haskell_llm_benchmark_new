module Proverb (recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xs = intercalate "\n" (verses ++ [finalLine])
  where
    pairs = zip xs (tail xs)
    verses =
      [ "For want of a " ++ a ++ " the " ++ b ++ " was lost."
      | (a, b) <- pairs
      ]
    finalLine = "And all for the want of a " ++ head xs ++ "."
