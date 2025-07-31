module OCR (convert) where

import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe)

convert :: String -> String
convert input =
  let ls = lines input
  in case validateDimensions ls of
       Just err -> error err
       Nothing  -> intercalate "," (map convertLine (chunk4 ls))

-- Validate that number of rows is multiple of 4 and each row length is multiple of 3
validateDimensions :: [String] -> Maybe String
validateDimensions rows
  | null rows = Just "invalid input"
  | length rows `mod` 4 /= 0 = Just "invalid input"
  | any (\r -> length r `mod` 3 /= 0) rows = Just "invalid input"
  | otherwise = Nothing

-- Break list into chunks of 4 rows
chunk4 :: [a] -> [[a]]
chunk4 [] = []
chunk4 xs =
  let (a,b) = splitAt 4 xs
  in a : chunk4 b

-- Convert a 4-row block into a string of digits
convertLine :: [String] -> String
convertLine rows4 =
  let cols = map (chunksOf 3) rows4 -- [[chunkedRow1], [chunkedRow2], [chunkedRow3], [chunkedRow4]]
      -- transpose to get per-digit chunks, each is [row1Chunk,row2Chunk,row3Chunk,row4Chunk]
      perDigit = transpose cols
  in map glyphToChar perDigit

-- Split a list into fixed-size chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (a,b) = splitAt n xs
  in a : chunksOf n b

glyphToChar :: [String] -> Char
glyphToChar g = fromMaybe '?' (lookup g digitMap)

-- Define the 3x4 glyphs for digits 0-9
digitMap :: [([String], Char)]
digitMap =
  [ ( [" _ "
      ,"| |"
      ,"|_|"
      ,"   "] , '0')
  , ( ["   "
      ,"  |"
      ,"  |"
      ,"   "] , '1')
  , ( [" _ "
      ," _|"
      ,"|_ "
      ,"   "] , '2')
  , ( [" _ "
      ," _|"
      ," _|"
      ,"   "] , '3')
  , ( ["   "
      ,"|_|"
      ,"  |"
      ,"   "] , '4')
  , ( [" _ "
      ,"|_ "
      ," _|"
      ,"   "] , '5')
  , ( [" _ "
      ,"|_ "
      ,"|_|"
      ,"   "] , '6')
  , ( [" _ "
      ,"  |"
      ,"  |"
      ,"   "] , '7')
  , ( [" _ "
      ,"|_|"
      ,"|_|"
      ,"   "] , '8')
  , ( [" _ "
      ,"|_|"
      ," _|"
      ,"   "] , '9')
  ]
