module OCR (convert) where

import Data.List (intercalate, dropWhile)

-- | Converts a string containing one or more OCR numbers into a comma-separated string of digits.
-- Each number is represented by 4 lines of text. Numbers are separated by blank lines.
-- Unrecognizable numbers are represented by '?'.
-- Malformed input (incorrect size) will result in an error.
convert :: String -> String
convert s = intercalate "," $ map convertSingle (numberBlocks s)

-- | Splits the full input string into a list of strings, where each string
-- represents a single number (4 lines of text). Handles blank lines between numbers.
numberBlocks :: String -> [String]
numberBlocks "" = []
numberBlocks s =
    let allLines = lines s
        (block, rest) = splitAt 4 allLines
    in if length block < 4
       then error "Incorrect size"
       else unlines block : numberBlocks (unlines (dropWhile null rest))

-- | Converts a single 4-line OCR string into a string of digits.
-- Validates that the input is 4 lines high and has a width that is a multiple of 3.
convertSingle :: String -> String
convertSingle s =
    let rows = lines s
    in if length rows /= 4 || any (\r -> length r `mod` 3 /= 0) rows
       then error "Incorrect size"
       else
            let width = length (head rows)
                indices = [0, 3 .. width - 3]
                getDigit i = map (\row -> take 3 (drop i row)) rows
            in map recognize (map getDigit indices)

-- | Recognizes a single digit from its 3x4 grid representation.
-- Returns '?' if the pattern is not recognized.
recognize :: [String] -> Char
recognize [" _ ",
           "| |",
           "|_|",
           "   "] = '0'
recognize ["   ",
           "  |",
           "  |",
           "   "] = '1'
recognize [" _ ",
           " _|",
           "|_ ",
           "   "] = '2'
recognize [" _ ",
           " _|",
           " _|",
           "   "] = '3'
recognize ["   ",
           "|_|",
           "  |",
           "   "] = '4'
recognize [" _ ",
           "|_ ",
           " _|",
           "   "] = '5'
recognize [" _ ",
           "|_ ",
           "|_|",
           "   "] = '6'
recognize [" _ ",
           "  |",
           "  |",
           "   "] = '7'
recognize [" _ ",
           "|_|",
           "|_|",
           "   "] = '8'
recognize [" _ ",
           "|_|",
           " _|",
           "   "] = '9'
recognize _ = '?'
