module OCR (convert) where

import Data.List (intercalate)

-- | Convert an entire OCR input (possibly multiple lines) into digits.
--   Blocks of 4 rows each represent one line of numbers; lines are joined with commas.
--   Raises an error on invalid input dimensions.
convert :: String -> String
convert input =
  let ls = lines input
  in if length ls `mod` 4 /= 0
       then error "invalid input"
       else
         let blocks = chunksOf 4 ls
             converted = map convertBlock blocks
         in intercalate "," converted

-- | Convert a single 4-row block into a string of digits (or '?' for unrecognized patterns).
convertBlock :: [String] -> String
convertBlock [l1, l2, l3, l4]
  | not (allEqual widths) || width `mod` 3 /= 0 = error "invalid input"
  | otherwise =
      let cols = width `div` 3
          -- Extract each 3×4 character cell
          cells = [ [ take 3 . drop (3 * k) $ row | row <- [l1, l2, l3, l4] ]
                  | k <- [0 .. cols - 1] ]
      in map recognize cells
  where
    widths = map length [l1, l2, l3, l4]
    width = head widths

-- | Recognize one 3×4 cell; unknown patterns map to '?'.
recognize :: [String] -> Char
recognize cell = case lookup cell patterns of
                   Just d  -> d
                   Nothing -> '?'

-- | Known OCR patterns for digits 0–9.
patterns :: [([String], Char)]
patterns =
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

-- | Split a list into chunks of a given size; error if trailing remainder.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs >= n = take n xs : chunksOf n (drop n xs)
  | otherwise      = error "invalid input"

-- | True if all elements of the list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs
