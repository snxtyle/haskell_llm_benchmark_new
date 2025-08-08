{-# LANGUAGE BangPatterns #-}

module Octal (readOct, showOct) where

-- Convert an octal string to its decimal Integer equivalent.
-- Returns 0 if the input contains any non-octal character.
readOct :: String -> Integer
readOct [] = 0
readOct xs = go 0 xs
  where
    go :: Integer -> String -> Integer
    go !acc [] = acc
    go !acc (c:cs)
      | '0' <= c && c <= '7' =
          let d    = toInteger (fromEnum c - fromEnum '0')
              acc' = acc * 8 + d
          in go acc' cs
      | otherwise = 0

-- Convert an integral value to its octal string representation.
-- Negative numbers are handled by prefixing a minus sign.
showOct :: Integral a => a -> String
showOct n
  | n < 0     = '-' : showOct (negate n)
  | n == 0    = "0"
  | otherwise = reverse (digits n)
  where
    digits :: Integral a => a -> String
    digits 0 = ""
    digits m =
      let (q, r) = m `divMod` 8
          c      = toEnum (fromEnum '0' + fromIntegral r) :: Char
      in c : digits q
