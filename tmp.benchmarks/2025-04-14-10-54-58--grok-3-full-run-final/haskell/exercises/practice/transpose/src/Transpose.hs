module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose rows = map (extractColumn paddedRows) [0..maxLength-1]
  where
    -- Find the length of the longest row
    maxLength = maximum (map length rows ++ [0])
    -- Pad shorter rows to the left with spaces
    paddedRows = map (padLeft maxLength) rows
    -- Extract a specific column as a string
    extractColumn rs col = map (\r -> if col < length r then r !! col else ' ') rs
    -- Pad a string to the left with spaces to reach desired length
    padLeft n s = replicate (n - length s) ' ' ++ s
