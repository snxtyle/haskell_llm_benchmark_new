module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose inputLines = transposeHelper paddedLines
  where
    -- Find the maximum length among all lines
    maxLength = maximum (0 : map length inputLines)
    
    -- Pad each line appropriately
    -- A line needs padding only if there's a longer line somewhere after it
    paddedLines = padLines inputLines
    
    -- Helper to recursively build the transposed result
    transposeHelper :: [String] -> [String]
    transposeHelper ls
      | all null ls = []
      | otherwise = map headOrSpace ls : transposeHelper (map tailOrEmpty ls)
    
    -- Get the first character or space if empty
    headOrSpace :: String -> Char
    headOrSpace [] = ' '
    headOrSpace (x:_) = x
    
    -- Get the tail or empty string if already empty
    tailOrEmpty :: String -> String
    tailOrEmpty [] = []
    tailOrEmpty (_:xs) = xs
    
    -- Pad lines based on the rule:
    -- A line needs padding with spaces only if there's a longer line after it
    padLines :: [String] -> [String]
    padLines [] = []
    padLines xs = zipWith padLine xs (tail (scanr max 0 (map length xs ++ [0])))
    
    -- Pad a single line to the required length
    padLine :: String -> Int -> String
    padLine str requiredLen = str ++ replicate (requiredLen - length str) ' '
