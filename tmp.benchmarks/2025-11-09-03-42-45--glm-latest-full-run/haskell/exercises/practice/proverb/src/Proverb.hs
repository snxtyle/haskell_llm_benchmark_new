module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite items =
  let
    proverbLines = zipWith (\a b -> "For want of a " ++ a ++ " the " ++ b ++ " was lost.") items (tail items)
    finalLine = "And all for the want of a " ++ head items ++ "."
  in
  unlines (proverbLines ++ [finalLine])
