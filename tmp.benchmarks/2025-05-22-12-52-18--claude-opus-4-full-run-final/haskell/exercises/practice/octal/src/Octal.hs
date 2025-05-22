module Octal (readOct, showOct) where

-- Convert an octal string to decimal
readOct :: String -> Integer
readOct "" = 0  -- Empty string is treated as 0
readOct str = if isValidOctal str
               then octalToDecimal str
               else 0  -- Invalid input is treated as 0

-- Check if a string contains only valid octal digits (0-7)
isValidOctal :: String -> Bool
isValidOctal [] = True
isValidOctal (c:cs) = c >= '0' && c <= '7' && isValidOctal cs

-- Convert a valid octal string to decimal
octalToDecimal :: String -> Integer
octalToDecimal str = octalToDecimal' (reverse str) 0
  where
    octalToDecimal' :: String -> Integer -> Integer
    octalToDecimal' [] _ = 0
    octalToDecimal' (c:cs) power = 
      let digit = fromIntegral (fromEnum c - fromEnum '0')
          value = digit * (8 ^ power)
      in value + octalToDecimal' cs (power + 1)

-- Convert a decimal number to octal string
showOct :: Integer -> String
showOct 0 = "0"
showOct n | n < 0 = error "Negative numbers not supported"
          | otherwise = reverse (showOct' n)
  where
    showOct' :: Integer -> String
    showOct' 0 = ""
    showOct' x = 
      let (quotient, remainder) = x `divMod` 8
      in toEnum (fromInteger remainder + fromEnum '0') : showOct' quotient
