module OCR (convert) where

import           Data.List       (intercalate)
import           Data.Maybe      (fromMaybe)

----------------------------------------------------------------------
-- Digit patterns
----------------------------------------------------------------------

-- | Association list mapping the 4‑row/3‑column patterns (each pattern
--   flattened to a 12‑character string) to their corresponding digits.
digitList :: [(String, Char)]
digitList =
  [ (concat [" _ "
            ,"| |"
            ,"|_|"
            ,"   "], '0')
  , (concat ["   "
            ,"  |"
            ,"  |"
            ,"   "], '1')
  , (concat [" _ "
            ," _|"
            ,"|_ "
            ,"   "], '2')
  , (concat [" _ "
            ," _|"
            ," _|"
            ,"   "], '3')
  , (concat ["   "
            ,"|_|"
            ,"  |"
            ,"   "], '4')
  , (concat [" _ "
            ,"|_ "
            ," _|"
            ,"   "], '5')
  , (concat [" _ "
            ,"|_ "
            ,"|_|"
            ,"   "], '6')
  , (concat [" _ "
            ,"  |"
            ,"  |"
            ,"   "], '7')
  , (concat [" _ "
            ,"|_|"
            ,"|_|"
            ,"   "], '8')
  , (concat [" _ "
            ,"|_|"
            ," _|"
            ,"   "], '9')
  ]

----------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------

-- split a list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n <= 0    = error "Chunk size must be positive"
  | otherwise = take n xs : chunksOf n (drop n xs)

-- pad a string on the right with spaces up to a given length
padRight :: Int -> String -> String
padRight n s
  | n <= length s = s
  | otherwise     = s ++ replicate (n - length s) ' '

-- extract a slice of 'len' characters starting at column 'start'
slice :: Int -> Int -> String -> String
slice start len s = take len . drop start $ (s ++ repeat ' ')

----------------------------------------------------------------------
-- public API
----------------------------------------------------------------------

-- | Convert the supplied ASCII representation into a sequence of digits.
--   Unknown patterns are rendered as '?'.
--   If the row count isn't a multiple of 4 or the column count isn't a
--   multiple of 3, we throw an error (as per exercise specification).
convert :: String -> String
convert input
  | null rows                 = ""
  | length rows `mod` 4 /= 0  = error "invalid row count"
  | otherwise                 = intercalate "," (map convertBlock blocks)
  where
    rows   = lines input
    blocks = chunksOf 4 rows        -- each block is 4 rows tall

    convertBlock :: [String] -> String
    convertBlock blk
      | width `mod` 3 /= 0 = error "invalid column count"
      | otherwise          = map (recogniseDigit blkPadded) [0 .. digitsCount - 1]
      where
        width        = maximum (map length blk)
        blkPadded    = map (padRight width) blk
        digitsCount  = width `div` 3

    recogniseDigit :: [String] -> Int -> Char
    recogniseDigit blk i = fromMaybe '?' (lookup pattern digitList)
      where
        start   = i * 3
        pattern = concat [ slice start 3 row | row <- blk ]
