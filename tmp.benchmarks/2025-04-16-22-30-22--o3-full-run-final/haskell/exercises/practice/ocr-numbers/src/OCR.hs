module OCR (convert) where

import Data.List (intercalate)

-- | Convert the OCR representation contained in the input string
--   to the corresponding digit characters.
--
--   * A 3 × 4 character cell represents one digit.
--   * Several digits can appear next to each other on the same line.
--   * Four text‑rows make one “OCR block”; several such blocks can be
--     stacked vertically, separated by a blank line that forms the
--     4th (blank) row of the previous block.
--   * If a cell cannot be recognised it becomes '?'.
--   * If the overall dimensions are wrong we raise an error.
convert :: String -> String
convert input =
  let ls = lines input
  in  if length ls `mod` 4 /= 0
         then error "invalid input"
         else intercalate "," . map convertBlock $ chunksOf4 ls
  where
    -- Split a list into chunks of exactly four elements.
    chunksOf4 :: [a] -> [[a]]
    chunksOf4 [] = []
    chunksOf4 xs =
      let (h, t) = splitAt 4 xs
      in  if length h < 4
            then error "invalid input"
            else h : chunksOf4 t

    -- Convert one 4‑row block into its digit string.
    convertBlock :: [String] -> String
    convertBlock rows4 =
      let width = maximum (map length rows4)
      in  if width `mod` 3 /= 0
            then error "invalid input"
            else
              let padded = map (padRight width) rows4
                  numDigits = width `div` 3
                  cells = [ digitAt i padded | i <- [0 .. numDigits - 1] ]
              in  map recognise cells

    -- Pad a row with spaces on the right so that all rows have the same width.
    padRight :: Int -> String -> String
    padRight n s = s ++ replicate (n - length s) ' '

    -- Extract the i‑th digit (0‑based) from a 4‑row block.
    digitAt :: Int -> [String] -> [String]
    digitAt i = map (take 3 . drop (i * 3))

    -- Turn the 3×4 matrix into the corresponding character.
    recognise :: [String] -> Char
    recognise cell = maybe '?' id (lookup cell digitPatterns)

-- The canonical patterns for the digits 0‑9.
digitPatterns :: [([String], Char)]
digitPatterns =
  [ ([" _ ",
     "| |",
     "|_|",
     "   "], '0')
  , (["   ",
     "  |",
     "  |",
     "   "], '1')
  , ([" _ ",
     " _|",
     "|_ ",
     "   "], '2')
  , ([" _ ",
     " _|",
     " _|",
     "   "], '3')
  , (["   ",
     "|_|",
     "  |",
     "   "], '4')
  , ([" _ ",
     "|_ ",
     " _|",
     "   "], '5')
  , ([" _ ",
     "|_ ",
     "|_|",
     "   "], '6')
  , ([" _ ",
     "  |",
     "  |",
     "   "], '7')
  , ([" _ ",
     "|_|",
     "|_|",
     "   "], '8')
  , ([" _ ",
     "|_|",
     " _|",
     "   "], '9')
  ]
