module Transpose (transpose) where

-- | Transpose a block of text represented as a list of lines.
--
--   * Rows become columns and columns become rows.
--   * When the input lines have different lengths we conceptually
--     pad the shorter ones on the /right/ with spaces so that the
--     matrix is rectangular.  
--   * Those padding spaces are kept only if they are followed (to
--     the right) by at least one real character.  In other words,
--     trailing “phantom” spaces produced by the padding are
--     dropped, while any space that was already present in the
--     original input is preserved.
--
--   Examples:
--
--   >>> transpose ["ABC", "DE"]
--   ["AD","BE","C"]
--
--   >>> transpose ["AB", "DEF"]
--   ["AD","BE"," F"]
--
transpose :: [String] -> [String]
transpose [] = []
transpose rows = [buildColumn c | c <- [0 .. maxLen - 1]]
  where
    -- Maximum length of any input row.
    maxLen :: Int
    maxLen = maximum (0 : map length rows)

    -- Safely obtain the character found at a given column of a row,
    -- or a space if the column is out of bounds for that row.
    charAt :: String -> Int -> Char
    charAt row col
      | col < length row = row !! col
      | otherwise        = ' '

    -- Construct one output row (i.e. one original column).
    buildColumn :: Int -> String
    buildColumn col =
      let lastRowIdx = findLastRowWithChar (length rows - 1)

          -- Rows that matter for this column: all rows up to and
          -- including the last one that really contributes a
          -- character.
          relevantRows = take (lastRowIdx + 1) rows
      in map (`charAt` col) relevantRows
      where
        -- Scan upwards from the bottom to locate the last row that
        -- has a real character (i.e. whose length exceeds the
        -- column index).
        findLastRowWithChar :: Int -> Int
        findLastRowWithChar idx
          | idx < 0                       = -1         -- should not occur
          | col < length (rows !! idx)    = idx
          | otherwise                     = findLastRowWithChar (idx - 1)
