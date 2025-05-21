module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite (x:xs) =
    let
        -- Create pairs of consecutive items for the intermediate lines
        -- e.g., for ["a", "b", "c"], this creates [("a", "b"), ("b", "c")]
        pairs = zip (x:xs) xs
        -- Generate the intermediate lines
        intermediateLines = map (\(a, b) -> "For want of a " ++ a ++ " the " ++ b ++ " was lost.") pairs
        -- Generate the final line using the first item of the original list
        finalLine = "And all for the want of a " ++ x ++ "."
    in
        -- Join all lines with newline characters, but without a trailing newline at the very end
        intercalate "\n" (intermediateLines ++ [finalLine])
