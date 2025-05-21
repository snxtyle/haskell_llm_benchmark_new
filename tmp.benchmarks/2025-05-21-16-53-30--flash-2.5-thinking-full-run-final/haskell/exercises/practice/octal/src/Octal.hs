module Octal (readOct, showOct) where

-- | Converts a character representing an octal digit ('0'-'7') to its integer value.
-- Uses fromEnum from Prelude to get the integer value of the character.
charToDigit :: Char -> Int
charToDigit c = fromEnum c - fromEnum '0'

-- | Checks if a character is a valid octal digit ('0'-'7').
isValidOctalDigit :: Char -> Bool
isValidOctalDigit c = c >= '0' && c <= '7'

-- | Converts an octal string to its decimal integer equivalent.
-- Returns 0 if the input string contains any non-octal digits.
readOct :: String -> Int
readOct s
  | all isValidOctalDigit s = foldl (\ !acc c -> acc * 8 + charToDigit c) 0 s
  | otherwise               = 0

-- | This function is not part of the current problem description.
showOct :: a -> String
showOct = error "You need to implement this function."
