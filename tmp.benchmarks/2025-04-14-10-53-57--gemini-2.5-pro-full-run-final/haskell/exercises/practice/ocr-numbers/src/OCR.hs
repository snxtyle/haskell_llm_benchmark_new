module OCR (convert) where

import Data.List (intercalate, transpose) -- Removed chunksOf from here

-- Helper function to split a list into chunks
-- Reimplemented here as Data.List.Split is not in base
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

-- Known digit patterns (3 rows, 3 columns)
digitPatterns :: [([String], Char)]
digitPatterns =
  [ ([" _ ", "| |", "|_|"], '0')
  , (["   ", "  |", "  |"], '1')
  , ([" _ ", " _|", "|_ "], '2')
  , ([" _ ", " _|", " _|"], '3')
  , (["   ", "|_|", "  |"], '4')
  , ([" _ ", "|_ ", " _|"], '5')
  , ([" _ ", "|_ ", "|_|"], '6')
  , ([" _ ", "  |", "  |"], '7')
  , ([" _ ", "|_|", "|_|"], '8')
  , ([" _ ", "|_|", " _|"], '9')
  ]

-- Recognizes a single digit pattern (list of 3 strings, each 3 chars long)
recognizeDigit :: [String] -> Char
recognizeDigit pattern =
  case lookup pattern digitPatterns of
    Just digit -> digit
    Nothing    -> '?'

-- Processes a single line of OCR digits (represented by 4 rows of text)
processDigitLine :: [String] -> String
processDigitLine rows
    -- Basic dimension checks
  | length rows /= 4 = error "Input line chunk must have 4 rows."
  | null rows || null (head rows) = "" -- Handle empty line case gracefully
  | any (/= firstLen) (map length rows) = error "All rows in a line chunk must have the same length."
  | firstLen `mod` 3 /= 0 = error "Row length must be a multiple of 3."
    -- Check if 4th row is blank, otherwise mark all digits as '?'
  | not (all (== ' ') (rows !! 3)) = replicate (firstLen `div` 3) '?'
    -- Process valid grid
  | otherwise = map recognizeDigit extractedDigits
  where
    firstLen = length (head rows)
    numDigits = firstLen `div` 3
    -- Take the first 3 rows for the actual pattern
    grid = take 3 rows
    -- Split each of the first 3 rows into 3-character chunks using the local helper
    chunkedRows = map (splitEvery 3) grid
    -- Transpose to group columns together. Each element of extractedDigits
    -- is now a list of 3 strings, representing one digit pattern.
    extractedDigits = transpose chunkedRows


-- Main conversion function
convert :: String -> String
convert input =
  let inputLines = lines input
      numLines = length inputLines
   in if numLines == 0
        then ""
        else if numLines `mod` 4 /= 0
               then error "Number of input lines must be a multiple of 4."
               else
                 -- Group lines into chunks of 4 using the local helper
                 let lineChunks = splitEvery 4 inputLines
                     -- Process each chunk
                     processedLines = map processDigitLine lineChunks
                  in -- Join the results with commas
                     intercalate "," processedLines
