module Octal (readOct, showOct) where

-- Convert octal string to decimal integer
readOct :: String -> Integer
readOct [] = 0
readOct str = if isValidOctal str then convertOctal str 0 else 0
  where
    isValidOctal = all (`elem` "01234567")
    
    convertOctal [] acc = acc
    convertOctal (c:cs) acc = 
      let digit = fromIntegral (fromEnum c - fromEnum '0')
          newAcc = acc * 8 + digit
      in newAcc `seq` convertOctal cs newAcc

-- Convert decimal integer to octal string
showOct :: Integer -> String
showOct 0 = "0"
showOct n 
  | n < 0 = error "Negative numbers not supported"
  | otherwise = reverse (toOctalDigits n)
  where
    toOctalDigits 0 = []
    toOctalDigits x = 
      let (q, r) = x `divMod` 8
          digit = toEnum (fromEnum '0' + fromIntegral r)
      in digit : toOctalDigits q
