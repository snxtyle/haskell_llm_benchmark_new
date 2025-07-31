module Octal (readOct, showOct) where

-- Convert an octal string to its decimal equivalent
readOct :: String -> Integer
readOct "" = 0
readOct str = readOctHelper str 0
  where
    readOctHelper [] acc = acc
    readOctHelper (c:cs) acc
      | c >= '0' && c <= '7' = readOctHelper cs (acc * 8 + fromIntegral (fromEnum c - fromEnum '0'))
      | otherwise = 0

-- Convert a decimal number to its octal string representation
showOct :: (Integral a) => a -> String
showOct 0 = "0"
showOct n = showOctHelper (fromIntegral n) ""
  where
    showOctHelper 0 acc = acc
    showOctHelper num acc = 
      let (quotient, remainder) = num `divMod` 8
      in showOctHelper quotient (toEnum (fromEnum '0' + fromInteger remainder) : acc)
