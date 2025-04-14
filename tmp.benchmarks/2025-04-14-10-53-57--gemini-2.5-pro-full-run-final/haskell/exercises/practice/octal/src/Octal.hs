{-# LANGUAGE BangPatterns #-} -- Enable BangPatterns extension

module Octal (readOct, showOct) where

import Prelude -- Explicitly import Prelude (standard practice)

-- Helper function to convert an octal character digit to its integer value.
-- Returns Nothing if the character is not a valid octal digit ('0' through '7').
-- Uses only functions from Prelude (fromEnum, comparison operators).
octDigitToInt :: Char -> Maybe Int
octDigitToInt c
  -- Check if the character code is within the range for '0' to '7'.
  | c >= '0' && c <= '7' = Just (fromEnum c - fromEnum '0')
  | otherwise            = Nothing

-- | Converts an octal string representation to its decimal integer equivalent.
--
--   The conversion follows the definition of octal numbers: each digit is
--   multiplied by the corresponding power of 8 (starting from 8^0 for the
--   rightmost digit).
--
--   Invalid input (strings containing characters other than '0'-'7', or empty
--   string) is treated as representing the decimal value 0.
--
--   Uses foldl and BangPatterns for efficiency and strictness.
readOct :: String -> Int
readOct octalStr =
  -- Use foldl (from Prelude) for left-associative fold.
  -- The accumulator 'acc' starts at 0 and holds the decimal value computed so far.
  -- We use -1 as an internal signal to indicate that invalid input has been detected.
  let result = foldl step 0 octalStr
  -- After the fold, check if the result is our error signal (-1).
  -- If it is, return 0 as required for invalid input. Otherwise, return the computed value.
  in if result == -1 then 0 else result
  where
    -- The step function for the fold. It takes the current accumulator (acc)
    -- and the next character (c) from the octal string.
    step :: Int -> Char -> Int
    step acc c
      -- If the accumulator is already -1, it means an invalid character was
      -- found earlier. Propagate the error signal.
      | acc == -1 = -1
      | otherwise =
          -- Try to convert the current character to an octal digit value.
          case octDigitToInt c of
            -- If the character is a valid octal digit:
            Just digit ->
              -- Calculate the new accumulator value.
              -- The BangPattern (!) ensures that 'newAcc' is evaluated strictly
              -- before the next step of the fold, helping to avoid space leaks
              -- even with the standard foldl.
              let !newAcc = acc * 8 + digit
              in newAcc
            -- If the character is not a valid octal digit (returns Nothing):
            Nothing ->
              -- Signal an error by returning -1. This will be propagated
              -- through the rest of the fold.
              -1

-- | Placeholder for the function to convert a decimal integer back to its
--   octal string representation. Implementation is not required by the current instructions.
showOct :: Int -> String
showOct = error "You need to implement this function."
