module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose xs
  | all null xs = []
  | otherwise = firstRow : transpose (map safeTail xs)
  where
    -- Get the first character of each non-empty row, pad with space for empty rows
    firstRow = map safeHead xs
    safeHead [] = ' '
    safeHead (c:_) = c
    safeTail [] = []
    safeTail (_:ys) = ys
