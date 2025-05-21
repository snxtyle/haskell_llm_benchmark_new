module OCR (convert) where

import Data.Map (Map, fromList, lookup)
import Data.List (intercalate) -- For Step 4: joining results with commas

-- Helper to split a list into chunks of a given size.
-- This is a common utility function, often found in Data.List.Split,
-- but we're implementing it manually to stick to standard libraries.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Define the patterns for digits 0-9.
-- Each pattern is a list of 4 strings, representing the 3x4 character grid.
digitPatterns :: Map [String] Char
digitPatterns = fromList
    [ ([" _ ",
        "| |",
        "|_|",
        "   "], '0')
    , (["   ",
        "  |",
        "  |",
        "   "], '1')
    , ([" _ ",
        " _|",
        "|_ ",
        "   "], '2')
    , ([" _ ",
        " _|",
        " _|",
        "   "], '3')
    , (["   ",
        "|_|",
        "  |",
        "   "], '4')
    , ([" _ ",
        "|_ ",
        " _|",
        "   "], '5')
    , ([" _ ",
        "|_ ",
        "|_|",
        "   "], '6')
    , ([" _ ",
        "  |",
        "  |",
        "   "], '7')
    , ([" _ ",
        "|_|",
        "|_|",
        "   "], '8')
    , ([" _ ",
        "|_|",
        " _|",
        "   "], '9')
    ]

-- Function to recognize a single 3x4 character pattern.
-- Returns the digit character or '?' if the pattern is not recognized.
recognizeDigit :: [String] -> Char
recognizeDigit pattern = case Data.Map.lookup pattern digitPatterns of
    Just digit -> digit
    Nothing    -> '?'

-- Function to extract a single 3x4 character pattern from a block of 4 lines.
-- 'block' is a list of 4 strings (lines) representing one or more numbers.
-- 'charIdx' is the 0-based index of the character to extract (e.g., 0 for the first, 1 for the second).
extractCharPattern :: [String] -> Int -> [String]
extractCharPattern block charIdx =
    let startCol = charIdx * 3
    in map (\line -> take 3 (drop startCol line)) block

-- Function to process a single 4-line block (e.g., a single "123" string).
-- It extracts all individual 3x4 character patterns from the block and converts them to a string of digits.
processBlock :: [String] -> String
processBlock block =
    let numChars = length (head block) `div` 3
        charIndices = [0 .. numChars - 1]
        patterns = map (extractCharPattern block) charIndices
    in map recognizeDigit patterns

-- Main conversion function.
-- Takes the raw OCR input string and returns the recognized numbers as a comma-separated string.
convert :: String -> String
convert input =
    let allLines = lines input
        numTotalLines = length allLines
        -- Determine the length of the first line. If input is empty, assume 0.
        firstLineLen = if null allLines then 0 else length (head allLines)
    in
    -- Validate input dimensions:
    -- 1. Total number of lines must be a multiple of 4 (each number block is 4 lines high).
    -- 2. Length of each line must be a multiple of 3 (each character is 3 columns wide).
    --    We only check the first line's length, assuming all lines have consistent length.
    if numTotalLines `mod` 4 /= 0 || firstLineLen `mod` 3 /= 0
    then error "Invalid input dimensions"
    else
        -- Split the input lines into blocks of 4 lines each.
        let lineBlocks = chunksOf 4 allLines
            -- Process each 4-line block to get its recognized number string.
            results = map processBlock lineBlocks
        -- Join the results of multiple blocks with commas (Step 4).
        in intercalate "," results
