module Transpose (transpose) where

-- | Transposes a list of strings, treating them as rows of a matrix.
--   Handles rows of different lengths by padding shorter rows with spaces
--   on the right during the transposition process, and then trimming
--   trailing spaces from the resulting rows, but only if there is more
--   than one input row.
transpose :: [String] -> [String]
transpose [] = []
transpose inputLines = map buildRow [0..maxLen-1]
  where
    -- The length of the longest line in the input.
    maxLen = maximum (map length inputLines)

    -- The number of rows in the input.
    numRows = length inputLines

    -- Builds a single row of the transposed matrix for a given column index,
    -- applying trimRight only if there are multiple input rows.
    buildRow :: Int -> String
    buildRow i = 
        let row = map (charAt i) inputLines
        in if numRows > 1 then trimRight row else row

    -- Gets the character at a specific index from a string, or a space
    -- if the index is out of bounds.
    charAt :: Int -> String -> Char
    charAt i s = if i < length s then s !! i else ' '

    -- Trims trailing spaces from a string.
    trimRight :: String -> String
    trimRight = reverse . dropWhile (== ' ') . reverse
