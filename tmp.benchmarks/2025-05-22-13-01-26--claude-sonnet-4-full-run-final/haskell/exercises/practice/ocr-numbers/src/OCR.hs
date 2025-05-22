module OCR (convert) where

import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)

convert :: String -> String
convert input
  | null input = ""
  | otherwise = 
    let inputLines = lines input
        numRows = length inputLines
    in if numRows `mod` 4 /= 0
       then error "Number of input rows is not a multiple of 4"
       else
         let groups = chunksOf 4 inputLines
             results = map convertGroup groups
         in intercalate "," results

convertGroup :: [String] -> String
convertGroup group
  | length group /= 4 = error "Each group must have exactly 4 rows"
  | not (all (\line -> length line `mod` 3 == 0) group) = 
      error "Each row width is not a multiple of 3"
  | otherwise =
    let width = if null group then 0 else length (head group)
        numDigits = width `div` 3
        digits = [extractDigit group i | i <- [0..numDigits-1]]
    in concatMap recognizeDigit digits

extractDigit :: [String] -> Int -> [String]
extractDigit rows digitIndex =
  let startCol = digitIndex * 3
      endCol = startCol + 3
  in map (take 3 . drop startCol . padTo (endCol)) rows
  where
    padTo n str = str ++ replicate (max 0 (n - length str)) ' '

recognizeDigit :: [String] -> String
recognizeDigit digit = 
  case digit of
    [" _ ", "| |", "|_|", "   "] -> "0"
    ["   ", "  |", "  |", "   "] -> "1"
    [" _ ", " _|", "|_ ", "   "] -> "2"
    [" _ ", " _|", " _|", "   "] -> "3"
    ["   ", "|_|", "  |", "   "] -> "4"
    [" _ ", "|_ ", " _|", "   "] -> "5"
    [" _ ", "|_ ", "|_|", "   "] -> "6"
    [" _ ", "  |", "  |", "   "] -> "7"
    [" _ ", "|_|", "|_|", "   "] -> "8"
    [" _ ", "|_|", " _|", "   "] -> "9"
    _ -> "?"
