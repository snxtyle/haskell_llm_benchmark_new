{-# LANGUAGE BangPatterns #-}

module Trinary (readTri, showTri) where

import Data.Char (digitToInt) -- Using digitToInt is more idiomatic than fromEnum

-- | Converts a trinary string to its decimal integer equivalent.
-- Invalid strings (containing characters other than '0', '1', or '2') result in 0.
readTri :: String -> Integer
readTri s
  | null s = 0
  | all (`elem` "012") s = go 0 s
  | otherwise = 0
  where
    -- A strict, tail-recursive helper to accumulate the decimal value.
    -- The bang pattern on 'acc' ensures that the intermediate calculation
    -- (acc * 3 + d) is evaluated at each step, preventing stack overflow
    -- from building up large thunks for long strings.
    go :: Integer -> String -> Integer
    go !acc [] = acc
    go !acc (c:cs) =
      let d = toInteger (digitToInt c) -- Converts '0'..'2' to 0..2
      in go (acc * 3 + d) cs

-- | Converts a non-negative decimal integer to its trinary string representation.
-- For negative numbers, it returns an empty string.
showTri :: Integer -> String
showTri num
  | num == 0 = "0"
  | num > 0  = go num ""
  | otherwise = "" -- Handles negative numbers as per unspecified behavior
  where
    -- A tail-recursive helper that builds the string in reverse order.
    -- This is more efficient than repeatedly using (++).
    go :: Integer -> String -> String
    go 0 acc = acc
    go n acc =
      let (q, r) = n `divMod` 3
          rChar = toEnum (fromInteger r + fromEnum '0') -- Converts 0..2 to '0'..'2'
      in go q (rChar : acc)
