module Octal (readOct, showOct) where

-- Convert an octal string to a decimal integer
-- Invalid input returns 0
readOct :: String -> Integer
readOct str = if isValidOctal str
              then octalToDecimal str
              else 0

-- Check if a string contains only valid octal digits (0-7)
isValidOctal :: String -> Bool
isValidOctal [] = False
isValidOctal xs = all isOctalDigit xs

-- Check if a character is a valid octal digit
isOctalDigit :: Char -> Bool
isOctalDigit c = c >= '0' && c <= '7'

-- Convert a valid octal string to decimal
octalToDecimal :: String -> Integer
octalToDecimal str = foldl (\acc digit -> acc * 8 + digitToInt digit) 0 str
  where
    digitToInt :: Char -> Integer
    digitToInt c = fromIntegral (fromEnum c - fromEnum '0')

-- Convert a decimal integer to an octal string
showOct :: Integer -> String
showOct 0 = "0"
showOct n = reverse (toOctalDigits n)
  where
    toOctalDigits :: Integer -> String
    toOctalDigits 0 = ""
    toOctalDigits x = 
      let (q, r) = x `divMod` 8
      in intToDigit r : toOctalDigits q
    
    intToDigit :: Integer -> Char
    intToDigit i = toEnum (fromInteger i + fromEnum '0')
