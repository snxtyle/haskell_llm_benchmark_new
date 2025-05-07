module OCR (convert) where

import Data.List (transpose, intercalate)

-- Helper function to split a list into chunks of a given size.
-- Equivalent to chunksOf from Data.List.Split.
customChunksOf :: Int -> [a] -> [[a]]
customChunksOf _ [] = []
customChunksOf n xs = take n xs : customChunksOf n (drop n xs)

-- Define the visual patterns for each digit.
-- Each pattern consists of 3 lines, each 3 characters wide.
-- The fourth line of an OCR digit is always blank and thus not needed for recognition.
digitPatterns :: [([String], Char)]
digitPatterns =
  [ ([" _ ",
      "| |",
      "|_|"], '0')
  , (["   ",
      "  |",
      "  |"], '1')
  , ([" _ ",
      " _|",
      "|_ "], '2')
  , ([" _ ",
      " _|",
      " _|"], '3')
  , (["   ",
      "|_|",
      "  |"], '4')
  , ([" _ ",
      "|_ ",
      " _|"], '5')
  , ([" _ ",
      "|_ ",
      "|_|"], '6')
  , ([" _ ",
      "  |",
      "  |"], '7')
  , ([" _ ",
      "|_|",
      "|_|"], '8')
  , ([" _ ",
      "|_|",
      " _|"], '9')
  ]

-- Recognizes a single digit pattern (3 lines of 3 chars each).
-- Returns the digit character or '?' if unrecognizable.
recognizeDigit :: [String] -> Char
recognizeDigit pattern =
  case lookup pattern digitPatterns of
    Just digitChar -> digitChar
    Nothing        -> '?'

-- Processes a single "line" of OCR digits.
-- An OCR "line" consists of 4 actual text lines.
processSingleOCRLine :: [String] -> String
processSingleOCRLine ocrLines
  -- This first check is more of an assertion, as convert should ensure 4 lines are passed.
  | length ocrLines /= 4 = error "Internal error: processSingleOCRLine expects 4 lines."
  | otherwise =
    let
      -- Determine the length of the first line. All lines in the block must match this.
      -- Handle case where ocrLines might be like ["","","",""]
      firstLine = head ocrLines
      firstLineLength = length firstLine

      -- Validate that all lines in the 4-line block have the same length.
      consistentLengths = all (\l -> length l == firstLineLength) (tail ocrLines)
      -- Validate that the determined line length is a multiple of 3.
      validWidth = firstLineLength `mod` 3 == 0
      -- Validate that the fourth line is blank (all spaces).
      -- It must also have the same length as other lines.
      fourthLine = ocrLines !! 3
      fourthLineBlank = length fourthLine == firstLineLength && all (== ' ') fourthLine

    in
      if not consistentLengths then
        error "All lines in a 4-line block must have the same length."
      else if not validWidth then
        error "Line length must be a multiple of 3."
      else if not fourthLineBlank then
        error "The fourth line of each digit block must be blank and match other line lengths."
      else
        if firstLineLength == 0 then -- Handles input like ["", "", "", ""] correctly as an empty string of digits.
          ""
        else
          let
            -- Take the first 3 lines, which contain the digit patterns.
            digitSignalRows = take 3 ocrLines
            -- Segment each of these 3 lines into 3-character wide chunks.
            -- e.g., [" _  _ ", "| || |", "|_||_|"] becomes
            -- [[" _ "," _ "],["| |","| |"],["|_|","|_|"]]
            segmentedRows = map (customChunksOf 3) digitSignalRows
            -- Transpose the segmented rows to group them by digit position.
            -- e.g., [[" _ ","| |","|_|"], [" _ ","| |","|_|"]]
            -- Each inner list is now a pattern for a single digit.
            columnarDigitPatterns = transpose segmentedRows
            -- Recognize each digit pattern.
          in map recognizeDigit columnarDigitPatterns

-- Main conversion function.
convert :: String -> String
convert input =
  let
    allInputLines = lines input
    numTotalLines = length allInputLines
  in
    if numTotalLines == 0 then
      -- An empty input string results in an empty output string.
      ""
    else if numTotalLines `mod` 4 /= 0 then
      -- Input must have a number of lines that is a multiple of 4.
      error "Input must have a number of lines that is a multiple of 4."
    else
      let
        -- Group lines into 4-line blocks. Each block is one OCR "line".
        ocrLineBlocks = customChunksOf 4 allInputLines
        -- Process each block to get its string representation (e.g., "123", "45?").
        parsedNumberStrings = map processSingleOCRLine ocrLineBlocks
      in
        -- Join the results from multiple OCR "lines" with commas.
        intercalate "," parsedNumberStrings
