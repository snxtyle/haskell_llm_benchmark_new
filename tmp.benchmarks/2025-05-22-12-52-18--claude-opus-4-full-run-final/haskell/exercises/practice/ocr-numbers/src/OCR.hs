module OCR (convert) where

import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe)

convert :: String -> String
convert input
  | null lines' = error "Invalid input: empty string"
  | any (\line -> length line `mod` 3 /= 0) lines' = error "Invalid input: width must be multiple of 3"
  | length lines' `mod` 4 /= 0 = error "Invalid input: height must be multiple of 4"
  | otherwise = intercalate "," $ map processLine $ groupLines lines'
  where
    lines' = lines input

-- Group lines into sets of 4 (one row of digits)
groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines ls = take 4 ls : groupLines (drop 4 ls)

-- Process a single line (4 rows) of digits
processLine :: [String] -> String
processLine fourLines = map recognizeDigit $ splitDigits fourLines

-- Split a 4-line group into individual 3-wide digits
splitDigits :: [String] -> [[String]]
splitDigits fourLines = transpose $ map (chunksOf 3) fourLines
  where
    chunksOf n [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Recognize a single digit from its 4x3 representation
recognizeDigit :: [String] -> Char
recognizeDigit digit = fromMaybe '?' $ lookup digit digitPatterns

-- Define the patterns for each digit
digitPatterns :: [([String], Char)]
digitPatterns =
  [ (["   ", "  |", "  |", "   "], '1')
  , ([" _ ", " _|", "|_ ", "   "], '2')
  , ([" _ ", " _|", " _|", "   "], '3')
  , (["   ", "|_|", "  |", "   "], '4')
  , ([" _ ", "|_ ", " _|", "   "], '5')
  , ([" _ ", "|_ ", "|_|", "   "], '6')
  , ([" _ ", "  |", "  |", "   "], '7')
  , ([" _ ", "|_|", "|_|", "   "], '8')
  , ([" _ ", "|_|", " _|", "   "], '9')
  , ([" _ ", "| |", "|_|", "   "], '0')
  ]
