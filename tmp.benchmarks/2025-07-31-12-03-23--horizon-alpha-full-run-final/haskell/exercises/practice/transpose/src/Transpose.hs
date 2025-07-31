module Transpose (transpose) where

-- Transpose a list of strings (rows) into a list of strings (columns).
-- Rules:
-- - If rows are of unequal length, conceptually pad on the left with spaces
--   to make them equal length before transposing.
-- - Do not pad on the right in the output: only remove trailing spaces that
--   come solely from padding of shorter input rows. Preserve spaces that are
--   actual characters in the input.
transpose :: [String] -> [String]
transpose rows =
  let maxLen = maximum (0 : map length rows)

      -- Character at position j, or space if out of bounds (padding).
      charAt :: Int -> String -> Char
      charAt j s = if j < length s then s !! j else ' '

      -- Number of rows that actually contribute a real character at column j.
      contributingCount :: Int -> Int
      contributingCount j = length (filter (\s -> j < length s) rows)

      -- Build the transposed row for column j.
      -- Take all characters at column j, then drop the trailing part that
      -- belongs only to non-contributing rows (pure padding). Do not trim
      -- spaces within the contributing portion, since they are real input.
      buildRow :: Int -> String
      buildRow j =
        let cs = map (charAt j) rows
            n  = contributingCount j
        in take n cs

  in [ buildRow j | j <- [0 .. maxLen - 1] ]
