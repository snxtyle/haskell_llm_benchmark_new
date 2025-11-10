module IsbnVerifier (isbn) where

import Data.Char (isDigit, toLower)

-- | Checks if a string is a valid ISBN-10.
isbn :: String -> Bool
isbn s = isValid (filter (/= '-') s)
  where
    -- Checks the validity of a cleaned string (no hyphens).
    isValid :: String -> Bool
    isValid str
      -- Must have exactly 10 characters.
      | length str /= 10 = False
      -- First 9 must be digits.
      | not (all isDigit (take 9 str)) = False
      -- Last must be a digit or 'x'/'X'.
      | not (isDigit (last str) || toLower (last str) == 'x') = False
      -- If all checks pass, calculate the checksum.
      | otherwise = calculateChecksum str `mod` 11 == 0

    -- Calculates the checksum for a valid string.
    calculateChecksum :: String -> Int
    calculateChecksum str = sum (zipWith (*) (map charToVal str) [10,9..1])

    -- Converts a character to its integer value.
    -- Assumes the character is valid (digit or 'x'/'X').
    charToVal :: Char -> Int
    charToVal c
      | isDigit c = fromEnum c - fromEnum '0'
      | otherwise = 10 -- 'x' or 'X'
