module OCR (convert) where

import Data.List (transpose, intercalate)

-- Convert a string representation of OCR numbers to actual numbers
convert :: String -> String
convert input = 
    let lines = splitIntoLines input
    in if validInput lines
       then intercalate "," (map processLine lines)
       else error "Invalid input size"

-- Split input string into lines (each line being a list of strings for each row of OCR digits)
splitIntoLines :: String -> [[String]]
splitIntoLines input = 
    let rows = lines input
    in if null rows || length rows `mod` 4 /= 0
       then error "Invalid input size"
       else groupEvery 4 rows

-- Group list into sublists of n elements
groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n xs = take n xs : groupEvery n (drop n xs)

-- Validate input dimensions
validInput :: [[String]] -> Bool
validInput [] = True
validInput (line:rest) = all (\rows -> length rows == 4 && all (\r -> length r `mod` 3 == 0) rows) (line:rest)

-- Process a single line of OCR digits (4 rows)
processLine :: [String] -> String
processLine rows = 
    let chunkedRows = map (chunk 3) rows
        digits = transpose chunkedRows
    in concatMap (recognizeDigit . concatMap concat) digits

-- Split a string into chunks of size n
chunk :: Int -> String -> [String]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Recognize a single digit from its 3x4 grid representation
recognizeDigit :: String -> Char
recognizeDigit grid = case grid of
    " _ | ||_|   " -> '0'
    "     |  |   " -> '1'
    " _  _||_    " -> '2'
    " _  _| _|   " -> '3'
    "   |_|  |   " -> '4'
    " _ |_  _|   " -> '5'
    " _ |_ |_|   " -> '6'
    " _   |  |   " -> '7'
    " _ |_|_|    " -> '8'
    " _ |_| _|   " -> '9'
    _              -> '?'
