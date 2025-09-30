module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose inputLines = 
  let maxLen = maximum (0 : map length inputLines)
      padded = map (padLeft maxLen) inputLines
  in trimRight $ transposeEqual padded
  where
    -- Pad a string to the left with spaces to reach the target length
    padLeft :: Int -> String -> String
    padLeft n s = replicate (n - length s) ' ' ++ s
    
    -- Transpose lines that are all the same length
    transposeEqual :: [String] -> [String]
    transposeEqual [] = []
    transposeEqual ([]:_) = []
    transposeEqual xs = map safeHead xs : transposeEqual (map safeTail xs)
    
    -- Safe head that returns space for empty string (shouldn't happen with our logic)
    safeHead :: String -> Char
    safeHead [] = ' '
    safeHead (c:_) = c
    
    -- Safe tail that returns empty string for empty string
    safeTail :: String -> String
    safeTail [] = []
    safeTail (_:cs) = cs
    
    -- Remove trailing empty strings, but preserve spaces within strings
    trimRight :: [String] -> [String]
    trimRight = reverse . dropWhile null . reverse
