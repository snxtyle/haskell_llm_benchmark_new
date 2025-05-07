module Transpose (transpose) where

-- No special imports needed beyond Prelude for list functions like map, length, !!, maximum, foldr.

transpose :: [String] -> [String]
transpose [] = [] -- If the input is an empty list of strings, the output is also an empty list.
transpose lines =
    let
        -- Determine the number of lines in the input. This will be the maximum possible number of columns in the output.
        numInputLines = length lines

        -- Determine the maximum length of any line in the input.
        -- This will be the number of rows in the output.
        -- Since `lines` is guaranteed not to be empty here (due to the `transpose []` pattern match),
        -- `map length lines` will not be empty, so `maximum` is safe.
        -- If all lines are empty (e.g., ["", ""]), `map length` gives `[0,0]`, `maximum` is `0`.
        -- This means `[0 .. effectiveMaxLength - 1]` will be `[0 .. -1]`, which is `[]`,
        -- so `map buildOutputRow []` results in `[]`, which is correct for inputs like `["", ""]`.
        effectiveMaxLength = maximum (map length lines)

        -- getEffectiveChar: Retrieves a character from a specific input line at a specific char index (column).
        -- If the char index is out of bounds for that line, it returns a space.
        -- This implicitly handles the "pad to the left with spaces" requirement when constructing output rows,
        -- as shorter input lines will contribute spaces if the output row needs to be longer due to other input lines
        -- (specifically, other input lines that appear later in the original list, corresponding to later characters in the new row).
        -- Parameters:
        --   lineIdx_r: The index of the line in `lines` (0-based row index from input).
        --   charIdx_c: The character index within that line (0-based column index from input).
        getEffectiveChar :: Int -> Int -> Char
        getEffectiveChar lineIdx_r charIdx_c = -- `lines` is captured from the outer scope
            let currentLine = lines !! lineIdx_r
            in if charIdx_c < length currentLine
               then currentLine !! charIdx_c
               else ' '

        -- findRelevantLength: Determines the necessary length of a given output row.
        -- An output row (derived from column `charIdx_c` of the input) should extend
        -- up to the position corresponding to the last input line that actually contributes a character
        -- (not padding) at this `charIdx_c`. This implements the "Don't pad to the right" for output rows.
        -- Parameters:
        --   charIdx_c: The character index (column) from input being considered. This corresponds to an output row.
        findRelevantLength :: Int -> Int
        findRelevantLength charIdx_c = -- `lines` and `numInputLines` are captured
            foldr (\lineIdx_r currentMaxLen ->
                -- If the input line `lines !! lineIdx_r` has a character at `charIdx_c`
                if charIdx_c < length (lines !! lineIdx_r)
                -- Then this output row must be at least `lineIdx_r + 1` long.
                then max currentMaxLen (lineIdx_r + 1)
                -- Otherwise, this input line (at this `charIdx_c`) doesn't extend the required length for this output row.
                else currentMaxLen
            ) 0 [0 .. numInputLines - 1]

        -- buildOutputRow: Constructs a single row of the transposed output.
        -- This row corresponds to all characters at a specific column (`charIdx_c`) of the input lines.
        -- Parameter:
        --   charIdx_c: The column index from the input (0-based). This determines which characters to pick.
        buildOutputRow :: Int -> String
        buildOutputRow charIdx_c =
            let
                -- Determine how long this specific output row needs to be.
                relevantLen = findRelevantLength charIdx_c
            in
                -- If relevantLen is 0, it means no input line contributes a character at this `charIdx_c`
                -- that would require this output row to exist. This situation implies that `effectiveMaxLength`
                -- should have been 0, which is handled by `map buildOutputRow []` resulting in `[]`.
                -- If `effectiveMaxLength > 0`, then `relevantLen` for any `charIdx_c` in range should also be > 0.
                if relevantLen == 0
                then "" -- This output row is empty.
                else
                    -- Construct the output row by taking the `charIdx_c`-th character from each relevant input line.
                    -- `lineIdx_r` iterates from `0` to `relevantLen - 1`.
                    [getEffectiveChar lineIdx_r charIdx_c | lineIdx_r <- [0 .. relevantLen - 1]]

    -- The main transposition:
    -- For each column index `c` from `0` to `effectiveMaxLength - 1`, build an output row.
    -- `effectiveMaxLength` is the number of rows in the output.
    in map buildOutputRow [0 .. effectiveMaxLength - 1]
