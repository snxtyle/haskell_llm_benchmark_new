{-# LANGUAGE BangPatterns #-}

module Octal (readOct, showOct) where

import Data.Char (digitToInt, intToDigit)

-- | Convert a string representing an octal number to its decimal integer value.
-- Invalid input (containing characters other than '0'-'7') results in 0.
readOct :: String -> Int
readOct s = go 0 s
  where
    -- Use a bang pattern on the accumulator to ensure strict evaluation.
    go :: Int -> String -> Int
    go !acc [] = acc
    go acc (c:cs)
      | c >= '0' && c <= '7' = go (acc * 8 + digitToInt c) cs
      | otherwise            = 0 -- Invalid character found

-- | Convert a decimal integer to its octal string representation.
showOct :: Int -> String
showOct n
  | n == 0    = "0"
  | n < 0     = '-' : showOctHelper (-n) ""
  | otherwise = showOctHelper n ""
  where
    -- Helper function is tail-recursive and strict in its arguments.
    showOctHelper :: Int -> String -> String
    showOctHelper 0 acc = acc
    showOctHelper n acc =
      let (q, r) = n `quotRem` 8
          -- Use a bang pattern on the remainder to ensure strict evaluation.
          !r' = r
      in showOctHelper q (intToDigit r' : acc)
