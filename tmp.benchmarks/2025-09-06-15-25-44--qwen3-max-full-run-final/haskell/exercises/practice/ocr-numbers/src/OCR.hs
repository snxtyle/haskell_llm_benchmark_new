module OCR (convert) where

import Data.List (transpose)

convert :: String -> String
convert input = 
    let linesOfDigits = splitIntoDigitGroups (lines input)
        convertedLines = map convertLine linesOfDigits
    in unwords $ intersperse "," convertedLines
  where
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs
    intersperse _ [] = []

-- Split the input into groups of 4 lines (each group represents a row of digits)
splitIntoDigitGroups :: [String] -> [[String]]
splitIntoDigitGroups [] = []
splitIntoDigitGroups xs = 
    let (group, rest) = splitAt 4 xs
        nextRest = dropWhile null rest  -- Skip empty lines between groups
    in group : splitIntoDigitGroups nextRest

-- Convert a single line (up to 4 rows) to a string of digits
convertLine :: [String] -> String
convertLine rows
    | length rows < 3 = ""  -- Handle case where we have fewer than 3 rows
    | any (\row -> length row `mod` 3 /= 0) (take 3 rows) = error "Incorrect size"
    | otherwise = map convertDigit digitPatterns
  where
    -- Group the line into individual digit patterns (each 3 columns wide)
    digitPatterns = transpose $ map (chunk 3) (take 3 rows)
    
    -- Split a list into chunks of size n
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

-- Convert a single digit pattern to a character
convertDigit :: [String] -> Char
convertDigit digit@[r1, r2, r3]
    | digit == [" _ ", "| |", "|_|"] = '0'
    | digit == ["   ", "  |", "  |"] = '1'
    | digit == [" _ ", " _|", "|_ "] = '2'
    | digit == [" _ ", " _|", " _|"] = '3'
    | digit == ["   ", "|_|", "  |"] = '4'
    | digit == [" _ ", "|_ ", " _|"] = '5'
    | digit == [" _ ", "|_ ", "|_|"] = '6'
    | digit == [" _ ", "  |", "  |"] = '7'
    | digit == [" _ ", "|_|", "|_|"] = '8'
    | digit == [" _ ", "|_|", " _|"] = '9'
    | otherwise = '?'
convertDigit _ = error "Incorrect size"
