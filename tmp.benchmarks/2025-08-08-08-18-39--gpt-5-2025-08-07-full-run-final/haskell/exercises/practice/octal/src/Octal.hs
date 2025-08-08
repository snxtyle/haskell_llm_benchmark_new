module Octal (readOct, showOct) where

-- Convert an octal string to a decimal number.
-- Invalid input returns 0.
readOct :: Integral a => String -> a
readOct = go 0
  where
    go acc [] = acc
    go acc (c:cs)
      | isOctDigit c =
          let acc' = acc * 8 + digitVal c
          in acc' `seq` go acc' cs
      | otherwise = 0

    isOctDigit :: Char -> Bool
    isOctDigit c = c >= '0' && c <= '7'

    digitVal :: Num a => Char -> a
    digitVal c = fromIntegral (fromEnum c - fromEnum '0')

-- Convert a decimal number to its octal string representation.
showOct :: Integral a => a -> String
showOct n0
  | n0 == 0   = "0"
  | n0 < 0    = '-' : showPos (negate (toInteger n0))
  | otherwise = showPos (toInteger n0)
  where
    showPos :: Integer -> String
    showPos x = reverse (digits x)

    digits :: Integer -> String
    digits x =
      let (q, r) = quotRem x 8
          c = toDigit r
      in if q == 0 then [c] else c : digits q

    toDigit :: Integer -> Char
    toDigit d = toEnum (fromEnum '0' + fromIntegral d)
