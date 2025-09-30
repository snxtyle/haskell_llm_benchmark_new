module OCR (convert) where

import Data.List (intercalate)

-- Define the patterns for digits 0-9 (3 columns x 4 rows each)
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

-- Implement chunksOf using standard library functions
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

convert :: String -> String
convert xs
  | null xs = ""
  | otherwise = 
      let linesOfInput = lines xs
          numLines = length linesOfInput
      in if numLines `mod` 4 /= 0
         then error "Number of input lines is not a multiple of 4"
         else
           let groups = chunksOf 4 linesOfInput
               results = map convertGroup groups
           in intercalate "," results

convertGroup :: [String] -> String
convertGroup fourLines
  | length fourLines /= 4 = error "Each entry must have 4 lines"
  | not (all (\line -> length line `mod` 3 == 0) fourLines) = 
      error "Number of input columns is not a multiple of 3"
  | not (allSameLength fourLines) = 
      error "All lines must have the same length"
  | otherwise =
      let lineLength = length (head fourLines)
          numDigits = lineLength `div` 3
          digits = [extractDigit fourLines i | i <- [0..numDigits-1]]
      in map recognizeDigit digits

allSameLength :: [String] -> Bool
allSameLength [] = True
allSameLength (x:xs) = all (\line -> length line == length x) xs

extractDigit :: [String] -> Int -> String
extractDigit fourLines digitIndex =
  let startCol = digitIndex * 3
      extractCols line = take 3 (drop startCol (padLine line))
      padLine line = line ++ repeat ' '
  in concatMap extractCols fourLines

recognizeDigit :: String -> Char
recognizeDigit pattern =
  case lookup pattern digitPatterns of
    Just digit -> digit
    Nothing -> '?'
