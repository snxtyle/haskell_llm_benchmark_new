module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct s = if all isValid s then go 0 (reverse s) else 0
  where
    isValid c = c >= '0' && c <= '7'
    go acc [] = acc
    go acc (c:cs) = let d = fromEnum c - fromEnum '0'
                    in acc `seq` go (acc * 8 + d) cs

showOct :: Integer -> String
showOct n
  | n < 0     = '-' : showOct (-n)
  | n == 0    = "0"
  | otherwise = go n ""
  where
    go 0 acc = acc
    go m acc = go (m `div` 8) (toEnum (fromEnum '0' + (m `mod` 8)) : acc)
