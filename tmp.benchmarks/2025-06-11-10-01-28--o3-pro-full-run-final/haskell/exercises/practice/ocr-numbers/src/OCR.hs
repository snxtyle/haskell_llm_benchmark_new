module OCR (convert) where

import Data.List (intercalate)

-- | Public API ---------------------------------------------------------------

-- | Convert an ASCII OCR representation to its corresponding
-- character string according to the specification.
convert :: String -> String
convert = intercalate ","                       -- join individual numbers
        . map decodeGroup                       -- decode each 4-row block
        . splitIntoGroups . lines               -- split at empty lines


-- | Internal helpers ---------------------------------------------------------

-- Split the complete input into groups of lines that represent one
-- multi-digit number.  Empty lines (length 0) separate the groups.
splitIntoGroups :: [String] -> [[String]]
splitIntoGroups = filter (not . null) . go
  where
    go [] = []
    go xs =
      let (grp, rest) = span (/= "") xs
          rest'       = dropWhile (== "") rest
      in  grp : go rest'

-- Decode exactly one block (possibly containing many digits).
-- The block can consist of either 3 rows (trailing blank line stripped)
-- or 4 rows (with an explicit blank line).
-- Raises an error if the block shape is invalid.
decodeGroup :: [String] -> String
decodeGroup rows
  | rowCount /= 3 && rowCount /= 4 = err "Invalid number of rows"
  | any ((/= width) . length) rows = err "Rows have differing widths"
  | width `mod` 3 /= 0             = err "Width not divisible by 3"
  | otherwise                      = map decodeDigit [0 .. digitsCount - 1]
  where
    rowCount    = length rows
    width       = if null rows then 0 else length (head rows)
    -- Ensure we always have 4 rows by appending a blank row when necessary.
    fullRows    = if rowCount == 3
                    then rows ++ [replicate width ' ']
                    else rows
    digitsCount = width `div` 3

    decodeDigit n =
      let patternRows = map (take 3 . drop (n * 3)) fullRows
      in patternToChar patternRows

-- Translate a single 3×4 pattern into the corresponding character.
-- Returns '?' when the pattern is not recognised.
patternToChar :: [String] -> Char
patternToChar patternRows =
  maybe '?' id $ lookup patternRows digitPatterns

-- List of reference patterns for the digits 0–9.
digitPatterns :: [([String], Char)]
digitPatterns =
  [ ([" _ "
     ,"| |"
     ,"|_|"
     ,"   "], '0')
  , (["   "
     ,"  |"
     ,"  |"
     ,"   "], '1')
  , ([" _ "
     ," _|"
     ,"|_ "
     ,"   "], '2')
  , ([" _ "
     ," _|"
     ," _|"
     ,"   "], '3')
  , (["   "
     ,"|_|"
     ,"  |"
     ,"   "], '4')
  , ([" _ "
     ,"|_ "
     ," _|"
     ,"   "], '5')
  , ([" _ "
     ,"|_ "
     ,"|_|"
     ,"   "], '6')
  , ([" _ "
     ,"  |"
     ,"  |"
     ,"   "], '7')
  , ([" _ "
     ,"|_|"
     ,"|_|"
     ,"   "], '8')
  , ([" _ "
     ,"|_|"
     ," _|"
     ,"   "], '9')
  ]

-- Helper to shorten repetitive error creation.
err :: String -> a
err = error
