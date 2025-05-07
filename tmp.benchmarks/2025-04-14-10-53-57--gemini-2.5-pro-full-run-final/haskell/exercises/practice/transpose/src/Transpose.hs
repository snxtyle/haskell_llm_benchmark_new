module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose lines =
    -- Calculate the maximum length of the input strings.
    -- Use '0 :' to handle the case where 'lines' is not empty but contains only empty strings.
    let maxLength = maximum (0 : map length lines)
        numRows = length lines

        -- Safely get a character from the input grid at (row, column).
        -- Returns Nothing if the indices are out of bounds.
        safeGet :: Int -> Int -> Maybe Char
        safeGet r c
            | r >= 0 && r < numRows && c >= 0 && c < length (lines !! r) = Just ((lines !! r) !! c)
            | otherwise = Nothing

        -- Determine the character for the transposed grid at (original column c, original row r).
        -- This function handles the padding logic.
        getTransposedChar :: Int -> Int -> Maybe Char
        getTransposedChar c r = -- c is original column index, r is original row index
            case safeGet r c of
                Just char -> Just char -- Character exists at (r, c).
                Nothing   -> -- Character does not exist at (r, c).
                    -- Check if padding is needed by looking at rows below r.
                    -- Padding (' ') is needed if any row k > r extends to or past column c.
                    let needsSpace = any (\k -> c < length (lines !! k)) [(r + 1)..(numRows - 1)]
                    in if needsSpace then Just ' ' else Nothing

        -- Build a single row of the transposed output.
        -- This corresponds to a single column 'c' from the original input.
        buildTransposedRow :: Int -> String
        buildTransposedRow c = go 0
          where
            -- Recursively build the row by processing each original row index 'r'.
            go r
              | r >= numRows = "" -- Base case: processed all original rows for this column.
              | otherwise =
                  case getTransposedChar c r of
                    Just char -> char : go (r + 1) -- Prepend the character and recurse.
                    Nothing   -> "" -- Stop building this row if no character or padding needed.

    -- Generate each row of the transposed output by mapping buildTransposedRow
    -- over all original column indices.
    in map buildTransposedRow [0..(maxLength - 1)]
