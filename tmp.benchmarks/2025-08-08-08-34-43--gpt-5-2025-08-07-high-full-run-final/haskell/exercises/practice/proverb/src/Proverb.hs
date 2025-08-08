module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xs =
  let pairs = zip xs (tail xs)
      midLines = map (\(a, b) -> "For want of a " ++ a ++ " the " ++ b ++ " was lost.") pairs
      finalLine = "And all for the want of a " ++ head xs ++ "."
  in intercalate "\n" (midLines ++ [finalLine])
