module Transpose (transpose) where

-- No specific imports needed beyond Prelude for this implementation.

transpose :: [String] -> [String]
transpose [] = [] -- Base case for empty input
transpose lines =
    let
        -- Determine the maximum length of input lines.
        -- `maximum (0 : map length lines)` handles the case where `lines` is empty
        -- by ensuring `maxLength` is at least 0.
        maxLength = maximum (0 : map length lines)
        numRows = length lines

        -- Helper to get a character at a specific column index from a line,
        -- along with a boolean indicating if it's a padding space.
        -- The tuple (Char, Bool) represents (character, is_padded_space).
        getCharWithPadding :: Int -> String -> (Char, Bool)
        getCharWithPadding colIdx line =
            if colIdx < length line
                then (line !! colIdx, False) -- Character from input, not padded
                else (' ', True)             -- Padding space

        -- Generate the raw transposed columns.
        -- Each element in `rawTransposed` is a list of (Char, Bool) tuples,
        -- representing a column from the original input. This list will become
        -- a row in the final output.
        rawTransposed :: [[(Char, Bool)]]
        rawTransposed =
            [ [ getCharWithPadding colIdx (lines !! rowIdx)
              | rowIdx <- [0 .. numRows - 1]
              ]
            | colIdx <- [0 .. maxLength - 1]
            ]

        -- Determine if a (Char, Bool) tuple represents a significant character.
        -- A character is significant if it's not a space, or if it's an
        -- explicit space from the input (not a padding space).
        isSignificant :: (Char, Bool) -> Bool
        isSignificant (c, isPadded) = c /= ' ' || not isPadded

        -- Process each raw transposed column (which becomes an output row)
        -- to trim trailing non-significant spaces.
        processOutputRow :: [(Char, Bool)] -> String
        processOutputRow charTuples =
            let
                -- Find the index of the last significant character in the list.
                -- If no significant character is found, `lastSignificantIdx` will be -1.
                findLastSignificant :: Int -> Int -> Int
                findLastSignificant idx acc
                    | idx < 0 = acc
                    | isSignificant (charTuples !! idx) = idx
                    | otherwise = findLastSignificant (idx - 1) acc

                lastSignificantIdx = findLastSignificant (length charTuples - 1) (-1)

                -- Take characters up to and including the last significant one.
                -- If `lastSignificantIdx` is -1 (no significant characters),
                -- `trimmedTuples` will be an empty list.
                trimmedTuples =
                    if lastSignificantIdx == -1
                        then []
                        else take (lastSignificantIdx + 1) charTuples
            in
                -- Extract only the characters from the (Char, Bool) tuples.
                map fst trimmedTuples
    in
        map processOutputRow rawTransposed
