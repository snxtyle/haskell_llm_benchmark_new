module Diamond (diamond) where

import Data.Char (ord, chr, isAsciiUpper)

-- | Helper function to construct a single row of the diamond.
--   'char' is the character for the current row (e.g., 'A', 'B', 'C').
--   'maxCharIdx' is the 0-based index of the widest character (e.g., 0 for 'A', 2 for 'C').
makeRow :: Char -> Int -> String
makeRow char maxCharIdx =
    let
        -- Calculate the 0-based index of the current character
        idx = ord char - ord 'A'
        -- Calculate the number of leading/trailing spaces for this row
        numOuterSpaces = maxCharIdx - idx
        outerSpaces = replicate numOuterSpaces ' '
    in
        if idx == 0 -- Special case for the 'A' row (center row for 'A', top/bottom for others)
        then outerSpaces ++ [char] ++ outerSpaces
        else -- For all other rows, there are two identical characters
            let
                -- Calculate the number of spaces between the two characters
                numInnerSpaces = 2 * idx - 1
                innerSpaces = replicate numInnerSpaces ' '
            in
                outerSpaces ++ [char] ++ innerSpaces ++ [char] ++ outerSpaces

-- | Generates a diamond shape from a given uppercase character.
--   Returns 'Nothing' if the input character is not an uppercase ASCII letter.
--   Returns 'Just [String]' where each string is a row of the diamond.
diamond :: Char -> Maybe [String]
diamond maxChar
    | not (isAsciiUpper maxChar) = Nothing -- Input must be an uppercase ASCII letter
    | otherwise =
        let
            -- Calculate the 0-based index of the widest character
            maxCharIdx = ord maxChar - ord 'A'
            -- Generate the sequence of characters for the top half of the diamond
            -- (from 'A' up to maxChar, inclusive)
            topHalfChars = [chr (ord 'A' + i) | i <- [0 .. maxCharIdx]]
            -- Generate the sequence of characters for the bottom half of the diamond
            -- (from the character just before maxChar down to 'A', exclusive of maxChar)
            bottomHalfChars = [chr (ord 'A' + i) | i <- [maxCharIdx - 1, maxCharIdx - 2 .. 0]]
            -- Combine to get the full sequence of characters that will form the diamond rows
            allDiamondChars = topHalfChars ++ bottomHalfChars
        in
            -- Map each character to its corresponding row string using the makeRow helper
            Just $ map (\c -> makeRow c maxCharIdx) allDiamondChars

