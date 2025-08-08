module OCR (convert) where

import Data.List (intercalate, transpose)

convert :: String -> String
convert xs = 
    case parseInput xs of
        Left err -> err
        Right digits -> digits

parseInput :: String -> Either String String
parseInput input = 
    let allLines = lines input
        lineGroups = groupLines allLines
    in if any (not . validLineGroup) lineGroups
       then Left "Error: Invalid input size"
       else Right $ intercalate "," $ map processLineGroup lineGroups

-- Group lines into sets of 4 (3 content lines + 1 blank line)
groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines ls = 
    let (group, rest) = splitAt 4 ls
    in if null group 
       then []
       else group : groupLines rest

-- Check if a group of lines is valid (4 lines, correct width)
validLineGroup :: [String] -> Bool
validLineGroup group = 
    length group == 4 && 
    all (\line -> length line `mod` 3 == 0) group &&
    all (not . null) (take 3 group)  -- First 3 lines should not be empty

-- Process a group of 4 lines into a string of digits
processLineGroup :: [String] -> String
processLineGroup group = 
    let width = maximum $ map length $ take 3 group
        -- Pad lines to same width
        paddedLines = map (padToWidth width) $ take 3 group
        -- Split into 3-character wide columns
        digitColumns = transpose $ map (chunksOf 3) paddedLines
        -- Convert each column set to a digit
    in map recognizeDigit digitColumns

-- Pad a string to the given width with spaces
padToWidth :: Int -> String -> String
padToWidth w s = s ++ replicate (w - length s) ' '

-- Split a string into chunks of size n
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = 
    let (chunk, rest) = splitAt n xs
    in chunk : chunksOf n rest

-- Recognize a single digit from its 3x3 pattern
recognizeDigit :: [String] -> Char
recognizeDigit [row1, row2, row3] = 
    case (row1, row2, row3) of
        (" _ ", "| |", "|_|") -> '0'
        ("   ", "  |", "  |") -> '1'
        (" _ ", " _|", "|_ ") -> '2'
        (" _ ", " _|", " _|") -> '3'
        ("   ", "|_|", "  |") -> '4'
        (" _ ", "|_ ", " _|") -> '5'
        (" _ ", "|_ ", "|_|") -> '6'
        (" _ ", "  |", "  |") -> '7'
        (" _ ", "|_|", "|_|") -> '8'
        (" _ ", "|_|", " _|") -> '9'
        _ -> '?'
recognizeDigit _ = '?'
