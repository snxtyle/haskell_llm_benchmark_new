module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)
import Data.Maybe (catMaybes)

isbn :: String -> Bool
isbn s =
  let
    -- Remove hyphens from the input string
    cleaned = filter (/= '-') s
  in
    -- An ISBN-10 must have exactly 10 characters after removing hyphens.
    if length cleaned /= 10
      then False
      else
        let
          -- The first 9 characters must be digits.
          -- We map them to Maybe Int, returning Nothing if not a digit.
          maybeFirstNineValues :: [Maybe Int]
          maybeFirstNineValues = map (\c -> if isDigit c then Just (digitToInt c) else Nothing) (take 9 cleaned)

          -- The 10th character (check digit) can be a digit or 'X'.
          -- We map it to Maybe Int, returning Nothing if invalid.
          maybeCheckDigitValue :: Maybe Int
          maybeCheckDigitValue =
            let lastChar = last cleaned
            in if isDigit lastChar
                 then Just (digitToInt lastChar)
                 else if lastChar == 'X'
                        then Just 10
                        else Nothing

          -- Check if all characters were successfully converted to valid ISBN values.
          -- This means no 'Nothing' values in maybeFirstNineValues and maybeCheckDigitValue is not Nothing.
          allCharsAreValid :: Bool
          allCharsAreValid = all (/= Nothing) maybeFirstNineValues && maybeCheckDigitValue /= Nothing

          -- If all characters are valid, extract their integer values.
          -- catMaybes safely extracts Just values, ignoring Nothings.
          -- Since we've already checked allCharsAreValid, we know there are no Nothings here.
          values :: [Int]
          values = catMaybes maybeFirstNineValues ++ catMaybes [maybeCheckDigitValue]
        in
          if allCharsAreValid
            then
              -- Calculate the weighted sum: d1*10 + d2*9 + ... + d10*1
              let weightedSum = sum $ zipWith (*) values [10, 9 .. 1]
              in weightedSum `mod` 11 == 0
            else
              -- If any character is invalid (e.g., 'A' in the middle, or 'Y' as check digit),
              -- or 'X' appears in the first 9 positions, it's not a valid ISBN.
              False
