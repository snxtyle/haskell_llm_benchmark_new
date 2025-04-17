module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose rows =
  let nrows = length rows
      maxLen = maximum (map length rows)
      rowsTransposed =
        [ [ charAt row i | row <- rows ]
        | i <- [0 .. maxLen - 1]
        ]
      charAt row i
        | i < length row = row !! i
        | otherwise      = ' '
      trimRow row
        | nrows > 1 = rtrim row
        | otherwise = row
      rtrim = reverse . dropWhile (== ' ') . reverse
  in map trimRow rowsTransposed
