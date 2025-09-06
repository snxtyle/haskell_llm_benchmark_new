module OCR (convert) where

import Data.List (transpose, unfoldr)

convert :: String -> String
convert input = 
    let linesOfDigits = splitIntoLines input
        convertedLines = map convertLine linesOfDigits
    in unwords $ intersperse "," convertedLines
  where
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs
    intersperse _ [] = []

-- Split the input into separate lines (each line contains 4 rows of digits)
splitIntoLines :: String -> [String]
splitIntoLines = filter (not . null) . unfoldr splitLine
  where
    splitLine [] = Nothing
    splitLine xs = 
        let (line, rest) = splitAt 4 xs
        in Just (line, dropWhile null rest)  -- Skip empty lines between groups

-- Convert a single line (4 rows) to a string of digits
convertLine :: [String] -> String
convertLine rows
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
