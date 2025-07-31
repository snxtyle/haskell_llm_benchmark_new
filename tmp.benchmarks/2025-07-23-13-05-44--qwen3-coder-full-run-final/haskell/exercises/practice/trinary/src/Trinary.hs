module Trinary (readTri, showTri) where

-- Convert a trinary string to its decimal equivalent
readTri :: String -> Integer
readTri = readTriHelper 0

-- Helper function that accumulates the result
readTriHelper :: Integer -> String -> Integer
readTriHelper acc [] = acc
readTriHelper acc (x:xs) = 
  let digit = charToInt x
  in if digit == -1
     then 0
     else readTriHelper (acc * 3 + fromIntegral digit) xs

-- Convert a character to its integer value, returning -1 for invalid digits
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt _ = -1

-- Convert a decimal number to its trinary string representation
showTri :: Integer -> String
showTri 0 = "0"
showTri n = showTriHelper n ""

-- Helper function for converting to trinary
showTriHelper :: Integer -> String -> String
showTriHelper 0 acc = acc
showTriHelper n acc = 
  let (quotient, remainder) = quotRem n 3
  in showTriHelper quotient (show (fromInteger remainder) ++ acc)
