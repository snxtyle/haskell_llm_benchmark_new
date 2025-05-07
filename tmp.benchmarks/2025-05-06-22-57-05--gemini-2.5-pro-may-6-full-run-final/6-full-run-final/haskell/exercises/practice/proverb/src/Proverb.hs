module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite items@(firstItem : _) =
    -- items is the full non-empty list, e.g., ["nail", "shoe", "horse"]
    -- firstItem is its head, e.g., "nail"

    -- Generate the intermediate lines: "For want of a X the Y was lost."
    -- This involves pairing consecutive items from the `items` list.
    -- e.g., for ["nail", "shoe", "horse"]:
    --   tail items gives ["shoe", "horse"]
    --   zip items (tail items) gives [("nail", "shoe"), ("shoe", "horse")]
    -- If `items` has only one element, e.g., ["nail"]:
    --   tail items gives []
    --   zip items (tail items) gives [], so intermediateLines will be empty.
    let
        intermediateLinePairs = zip items (tail items)
        intermediateLines = map (\(want, lost) -> "For want of a " ++ want ++ " the " ++ lost ++ " was lost.") intermediateLinePairs

        -- The final line of the proverb, using the very first item from the input list.
        concludingLine = "And all for the want of a " ++ firstItem ++ "."

        -- Combine the intermediate lines (if any) with the concluding line.
        -- If intermediateLines is empty (e.g., input was ["nail"]),
        -- allProverbLines will just be [concludingLine].
        allProverbLines = intermediateLines ++ [concludingLine]

    -- Join all lines with a newline character.
    -- If allProverbLines is ["And all for the want of a nail."],
    -- intercalate will produce "And all for the want of a nail." (no trailing newline).
    in intercalate "\n" allProverbLines
