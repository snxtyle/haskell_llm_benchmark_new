module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.Maybe (fromMaybe)

-- Each digit is represented as a list of 4 strings (rows)
digitMap :: [([String], Char)]
digitMap =
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

-- Map from digit representation to char
digitDict :: [String] -> Char
digitDict ds = fromMaybe '?' $ lookup ds digitMap

-- Split a string into chunks of n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Given 4 lines, split into digit blocks (each 3 columns wide)
parseDigits :: [String] -> [[String]]
parseDigits [a,b,c,d] =
  let as = chunksOf 3 a
      bs = chunksOf 3 b
      cs = chunksOf 3 c
      ds = chunksOf 3 d
  in zipWith4 (\x y z w -> [x,y,z,w]) as bs cs ds
  where
    zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
    zipWith4 _ _ _ _ _ = []
parseDigits _ = []

-- Validate input: must be multiple of 4 lines, each line same length, length multiple of 3
validateInput :: [String] -> Either String ()
validateInput xs
  | null xs = Right ()
  | any (/= head lens) lens = Left "All lines must be the same length."
  | (length xs `mod` 4) /= 0 = Left "Number of input lines is not a multiple of 4."
  | (head lens `mod` 3) /= 0 = Left "Line length is not a multiple of 3."
  | otherwise = Right ()
  where lens = map length xs

-- Convert a single 4-line block to a string of digits
convertBlock :: [String] -> String
convertBlock block =
  let digits = parseDigits block
  in map digitDict digits

-- Split input into blocks of 4 lines each
splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks xs = take 4 xs : splitBlocks (drop 4 xs)

convert :: String -> String
convert input =
  let ls = lines input
  in case validateInput ls of
    Left err -> error err
    Right () ->
      let blocks = splitBlocks ls
          converted = map convertBlock blocks
      in intercalate "," converted
