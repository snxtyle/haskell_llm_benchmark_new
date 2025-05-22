module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite items@(first:_) = intercalate "\n" (middleLines ++ [lastLine])
  where
    middleLines = zipWith makeLine items (tail items)
    makeLine item1 item2 = "For want of a " ++ item1 ++ " the " ++ item2 ++ " was lost."
    lastLine = "And all for the want of a " ++ first ++ "."
