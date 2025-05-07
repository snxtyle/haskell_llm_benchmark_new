{-# LANGUAGE BangPatterns #-}
module Octal (readOct, showOct) where

-- No explicit imports are needed as `all`, `fromEnum`, list operations,
-- and basic arithmetic are part of Prelude, which is imported by default.

readOct :: String -> Int
readOct octalStr
  -- Check if all characters in the string are valid octal digits.
  -- The `all` function is from Prelude.
  -- If the string is empty, `all isValidOctalDigit ""` is true.
  -- The `go` function correctly handles an empty string by returning the initial accumulator (0).
  | not (all isValidOctalDigit octalStr) = 0
  | otherwise                            = go 0 octalStr
  where
    -- Checks if a character is a valid octal digit ('0'-'7').
    isValidOctalDigit :: Char -> Bool
    isValidOctalDigit c = c >= '0' && c <= '7'

    -- Converts a valid octal character digit to its integer value.
    -- `fromEnum` is from Prelude (re-exported from GHC.Enum).
    octalCharToInt :: Char -> Int
    octalCharToInt c = fromEnum c - fromEnum '0'

    -- Tail-recursive helper function to perform the conversion.
    -- `!acc` uses a BangPattern to ensure the accumulator is strictly evaluated.
    go :: Int -> String -> Int
    go !acc [] = acc -- Base case: If the string is empty, return the accumulated value.
    go !acc (ch:chs) = -- Recursive case: Process the head character and recurse on the tail.
      let !digit = octalCharToInt ch       -- Convert char to int, strictly.
          !newAcc = acc * 8 + digit       -- Update accumulator, strictly.
      in go newAcc chs

-- showOct remains unimplemented as per the instructions.
showOct = error "You need to implement this function."
