module Proverb(recite) where

import Data.List (intercalate) -- Import intercalate for potentially cleaner multi-line joining

-- Generates the lines "For want of a <want> the <lost> was lost."
formatLine :: (String, String) -> String
formatLine (want, lost) = "For want of a " ++ want ++ " the " ++ lost ++ " was lost."

-- Generates the final line "And all for the want of a <item>."
formatFinalLine :: String -> String
formatFinalLine item = "And all for the want of a " ++ item ++ "."

recite :: [String] -> String
recite [] = "" -- Handle empty list case
recite [item] = formatFinalLine item -- Handle single item case: no intermediate lines, no unlines needed
recite ws@(first:_) = -- Handle lists with two or more items
    -- Create pairs of consecutive items (e.g., [a,b,c] -> [(a,b), (b,c)])
    let pairs = zip ws (tail ws)
        -- Generate the intermediate proverb lines from the pairs
        intermediateLines = map formatLine pairs
        -- Generate the concluding line using the very first item
        finalLine = formatFinalLine first
        -- Combine all lines
        allLines = intermediateLines ++ [finalLine]
    -- Join the lines with newlines. Using intercalate avoids the trailing newline issue of unlines.
    in intercalate "\n" allLines
