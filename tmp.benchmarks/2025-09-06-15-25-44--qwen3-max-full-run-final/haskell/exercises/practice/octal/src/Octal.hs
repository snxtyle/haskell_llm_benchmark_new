module Octal (readOct, showOct) where

-- Convert an octal string to its decimal equivalent
readOct :: String -> Integer
readOct str = if all isValidOctalDigit str
              then go 0 str
              else 0
  where
    isValidOctalDigit c = c >= '0' && c <= '7'
    go acc [] = acc
    go acc (x:xs) = let acc' = acc * 8 + digitToInt x
                    in acc' `seq` go acc' xs
    digitToInt c = fromInteger $ toInteger $ fromEnum c - fromEnum '0'

-- Convert a decimal number to its octal string representation
showOct :: Integer -> String
showOct n
    | n < 0 = '-' : showOct' (negate n)
    | n == 0 = "0"
    | otherwise = showOct' n
  where
    showOct' 0 = ""
    showOct' x = let (q, r) = x `quotRem` 8
                     digit = toDigit r
                 in showOct' q ++ [digit]
    toDigit d = toEnum (fromEnum '0' + fromInteger d)
