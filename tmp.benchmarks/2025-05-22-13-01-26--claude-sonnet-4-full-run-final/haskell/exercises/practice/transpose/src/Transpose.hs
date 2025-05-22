module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose inputLines = 
    let maxLen = maximum (0 : map length inputLines)
        paddedLines = map (padToLength maxLen) inputLines
        transposed = transposeMatrix paddedLines
    in removeTrailingEmpty transposed
  where
    padToLength n str = str ++ replicate (n - length str) ' '
    
    transposeMatrix [] = []
    transposeMatrix ([] : _) = []
    transposeMatrix rows = map head rows : transposeMatrix (map tail rows)
    
    removeTrailingEmpty = reverse . dropWhile null . reverse
