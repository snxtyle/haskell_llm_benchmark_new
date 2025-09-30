{-# LANGUAGE BangPatterns #-}

module Octal (readOct, showOct) where

-- Convert octal string to decimal integer
readOct :: String -> Integer
readOct "" = 0
readOct str 
  | not (all isOctalDigit str) = 0
  | otherwise = foldl' convertDigit 0 str
  where
    isOctalDigit c = c >= '0' && c <= '7'
    
    convertDigit !acc c = acc * 8 + toInteger (fromEnum c - fromEnum '0')
    
    foldl' :: (a -> b -> a) -> a -> [b] -> a
    foldl' f = go
      where
        go !acc [] = acc
        go !acc (x:xs) = go (f acc x) xs

-- Convert decimal integer to octal string
showOct :: Integer -> String
showOct 0 = "0"
showOct n
  | n < 0 = error "showOct: negative number"
  | otherwise = reverse (go n)
  where
    go 0 = ""
    go x = let (q, r) = x `quotRem` 8
           in toEnum (fromInteger r + fromEnum '0') : go x
