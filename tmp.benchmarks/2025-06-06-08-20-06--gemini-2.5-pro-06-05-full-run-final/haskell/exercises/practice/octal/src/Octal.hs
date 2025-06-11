{-# LANGUAGE BangPatterns #-}
module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct s
  | all isOctalDigit s = toDecimal s
  | otherwise          = 0
  where
    isOctalDigit :: Char -> Bool
    isOctalDigit c = c >= '0' && c <= '7'

    toDecimal :: String -> Integer
    toDecimal = go 0
      where
        go :: Integer -> String -> Integer
        go !acc []     = acc
        go !acc (x:xs) = go (acc * 8 + fromIntegral (fromEnum x - fromEnum '0')) xs

showOct :: Integer -> String
showOct = error "You need to implement this function."
