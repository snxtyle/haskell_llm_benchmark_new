module OCR (convert) where

import Data.List (intercalate)

convert :: String -> String
convert input =
  let ls = lines input  -- Split input into lines
  in if length ls `mod` 4 /= 0
     then error "Input must have a number of lines that is a multiple of 4"
     else
       let groups = [ take 4 (drop (i * 4) ls) | i <- [0 .. (length ls `div` 4) - 1] ]  -- Group into sets of 4 lines
           processGroup :: [String] -> String
           processGroup group
             | length group /= 4 = error "Each group must have exactly 4 lines"
             | not (all (\s -> length s == length (head group)) group) = error "Lines in a group must be of equal length"
             | otherwise =
                 let width = length (head group)
                     numDigits = width `div` 3
                 in if width `mod` 3 /= 0
                    then error "Line width must be a multiple of 3"
                    else concat [ [recognize [ take 3 (drop (j * 3) (group !! k)) | k <- [0..3] ] ] | j <- [0..numDigits-1] ]
           rowStrings = map processGroup groups  -- Process each group
       in intercalate "," rowStrings  -- Join rows with commas

recognize :: [String] -> Char  -- Takes a list of 4 strings, each 3 characters
recognize block
  | block == [" _ ", "| |", "|_|", "   "] = '0'
  | block == ["   ", "  |", "  |", "   "] = '1'
  | block == [" _ ", " _|", "|_ ", "   "] = '2'
  | block == [" _ ", " _|", " _|", "   "] = '3'
  | block == ["   ", "|_|", "  |", "   "] = '4'
  | block == [" _ ", "|_ ", " _|", "   "] = '5'
  | block == [" _ ", "|_ ", "|_|", "   "] = '6'
  | block == [" _ ", "  |", "  |", "   "] = '7'
  | block == [" _ ", "|_|", "|_|", "   "] = '8'
  | block == [" _ ", "|_|", " _|", "   "] = '9'
  | otherwise = '?'  -- Garbled digit
