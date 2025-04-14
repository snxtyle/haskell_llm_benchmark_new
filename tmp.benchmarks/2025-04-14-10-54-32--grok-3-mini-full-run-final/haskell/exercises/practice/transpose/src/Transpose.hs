module Transpose (transpose) where

import qualified Data.List as List

-- | Transposes a list of strings according to the specified rules.
-- Rows of different lengths are handled by treating missing characters as spaces,
-- and trailing spaces in each output row are trimmed.
transpose :: [String] -> [String]
transpose [] = []  -- Empty input results in empty output
transpose lines =
  let maxLength = if null lines then 0 else List.maximum (map length lines)
      -- Build a matrix padded with spaces
      paddedLines = map (\s -> take maxLength (s ++ repeat ' ')) lines
      -- Transpose the padded matrix
      transposedPadded = List.transpose paddedLines  -- This gives a list of strings with spaces
      -- Trim trailing spaces from each row
      trimTrailing = map (List.dropWhileEnd (== ' ')) transposedPadded
  in trimTrailing  -- Return the trimmed transposed matrix
