{-# LANGUAGE BangPatterns #-}

module Octal (readOct, showOct) where

-- | Convert an octal string to its decimal equivalent.
--   Invalid input (any digit not in '0'..'7') yields 0.
readOct :: Integral a => String -> a
readOct = go 0
  where
    go :: Integral a => a -> String -> a
    go !acc []     = acc
    go !_   (c:_)
      | c < '0' || c > '7' = 0
    go !acc (c:cs) =
      let !digit = fromIntegral (fromEnum c - fromEnum '0')  -- convert digit to `a`
          !acc'  = acc * 8 + digit
      in go acc' cs

-- | Convert a non-negative decimal number to its octal string.
--   Any input <= 0 yields "0".
showOct :: Integral a => a -> String
showOct n
  | n <= 0    = "0"
  | otherwise = go n []
  where
    go :: Integral a => a -> String -> String
    go 0 acc = acc
    go x acc =
      let (q, r) = x `quotRem` 8
          -- r is an `a`, convert back to Int for toEnum
          !digit = toEnum (fromIntegral r + fromEnum '0') :: Char
      in go q (digit : acc)
