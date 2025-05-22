module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose inputLines = 
    let maxLength = maximum (0 : map length inputLines)
        -- For each line, we need to pad it based on the maximum length of lines that come after it
        -- This ensures we preserve the structure when transposing
        paddedLines = padLines inputLines
    in if maxLength == 0 
       then []
       else map (collectColumn paddedLines) [0..maxLength-1]
  where
    -- Pad each line based on the maximum length of lines that come after it
    padLines :: [String] -> [String]
    padLines [] = []
    padLines (line:rest) = 
        let maxLengthAfter = if null rest then 0 else maximum (map length rest)
            paddedLine = line ++ replicate (max 0 (maxLengthAfter - length line)) ' '
        in paddedLine : padLines rest
    
    collectColumn :: [String] -> Int -> String
    collectColumn paddedLines colIdx = 
        [line !! colIdx | line <- paddedLines, colIdx < length line]
