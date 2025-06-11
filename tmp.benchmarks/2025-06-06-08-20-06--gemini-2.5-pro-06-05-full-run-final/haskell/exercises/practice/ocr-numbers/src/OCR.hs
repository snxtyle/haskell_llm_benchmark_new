module OCR (convert) where

import Data.List (intercalate, transpose)

-- A helper to split a list into chunks of a given size.
-- This is implemented here as it is not in the base library.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Converts a string representation of OCR digits into a string of numbers.
convert :: String -> String
convert input =
    let textLines = lines input
        banks = chunksOf 4 textLines
    in intercalate "," (map processBank banks)

-- | Processes a single "bank" of text lines, which represents one line of numbers.
-- A bank is expected to be 4 lines high.
processBank :: [String] -> String
processBank bankLines
    | length bankLines /= 4 = error "Invalid number of rows: must be a multiple of 4"
    | any ((/= w) . length) bankLines = error "Inconsistent line widths"
    | w `mod` 3 /= 0 = error "Line width must be a multiple of 3"
    | otherwise = map recognizeDigit (parseDigits bankLines)
  where
    -- head is safe here because the length check ensures bankLines is not empty.
    w = length $ head bankLines

-- | Parses a bank of lines into a list of individual digit patterns.
-- Each digit pattern is a list of 4 strings, each 3 characters wide.
parseDigits :: [String] -> [[String]]
parseDigits = transpose . map (chunksOf 3)

-- | Recognizes a single digit pattern.
-- The pattern is a list of strings representing the character grid.
-- It checks the first 3 lines of the pattern.
recognizeDigit :: [String] -> Char
recognizeDigit p =
    case take 3 p of
      [" _ ", "| |", "|_|"] -> '0'
      ["   ", "  |", "  |"] -> '1'
      [" _ ", " _|", "|_ "] -> '2'
      [" _ ", " _|", " _|"] -> '3'
      ["   ", "|_|", "  |"] -> '4'
      [" _ ", "|_ ", " _|" ] -> '5'
      [" _ ", "|_ ", "|_|" ] -> '6'
      [" _ ", "  |", "  |" ] -> '7'
      [" _ ", "|_|", "|_|" ] -> '8'
      [" _ ", "|_|", " _|" ] -> '9'
      _                      -> '?'
