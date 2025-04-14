module OCR (convert) where

import Data.List (transpose)
import Data.Maybe (fromMaybe)

-- Each digit is represented by 4 rows and 3 columns of characters.
-- The fourth row is always blank.
-- We will parse input lines into groups of 4 lines, then split each line into chunks of 3 characters,
-- then parse each chunk of 4 lines (3 chars each) into a digit.

-- Map of digit patterns to their corresponding digit character
digitPatterns :: [(String, Char)]
digitPatterns =
  [ (" _ | ||_|   ", '0')
  , ("     |  |   ", '1')
  , (" _  _||_    ", '2')
  , (" _  _| _|   ", '3')
  , ("   |_|  |   ", '4')
  , (" _ |_  _|   ", '5')
  , (" _ |_ |_|   ", '6')
  , (" _   |  |   ", '7')
  , (" _ |_||_|   ", '8')
  , (" _ |_| _|   ", '9')
  ]

-- Convert a 4x3 block of characters into a string key for lookup
blockToKey :: [String] -> String
blockToKey = concat

-- Parse a single digit block (4 lines of 3 chars each) into a digit char or '?'
parseDigit :: [String] -> Char
parseDigit block =
  fromMaybe '?' $ lookup (blockToKey block) digitPatterns

-- Split a string into chunks of n characters
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- Validate input lines: must be multiple of 4 lines, each line length multiple of 3
validateInput :: [String] -> Either String ()
validateInput ls
  | length ls `mod` 4 /= 0 = Left "Invalid number of lines"
  | any (\l -> length l `mod` 3 /= 0) ls = Left "Invalid line length"
  | otherwise = Right ()

-- Parse a single 4-line block into a string of digits
parseLine :: [String] -> String
parseLine ls =
  let n = length (head ls) `div` 3
      -- For each digit index, extract the 4 lines of 3 chars for that digit
      digitsBlocks = [ [ line !! (3*i) : line !! (3*i + 1) : line !! (3*i + 2) : [] | line <- ls ] | i <- [0..n-1] ]
  in map parseDigit digitsBlocks

-- Split input lines into groups of 4 lines (each group represents one line of digits)
splitLines :: [String] -> [[String]]
splitLines [] = []
splitLines ls = let (h, t) = splitAt 4 ls in h : splitLines t

-- Main convert function
convert :: String -> String
convert input =
  let ls = lines input
  in case validateInput ls of
       Left _ -> error "Invalid input"
       Right _ ->
         let groups = splitLines ls
             parsedLines = map parseLine groups
         in  concatWithComma parsedLines

-- Join multiple lines with commas
concatWithComma :: [String] -> String
concatWithComma = foldr1 (\a b -> a ++ "," ++ b)
