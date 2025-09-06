module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose inputLines = map trimTrailingSpaces transposed
  where
    maxCol = maximum (map length inputLines)
    transposed = [map (getCharAt col) inputLines | col <- [0..maxCol-1]]
    
    getCharAt :: Int -> String -> Char
    getCharAt col row
      | col < length row = row !! col
      | otherwise = ' '
    
    trimTrailingSpaces :: String -> String
    trimTrailingSpaces = reverse . dropWhile (== ' ') . reverse
