module Say (inEnglish) where

import Data.List (intercalate)

-- | Converts a non-negative integer into its English word representation.
-- Returns Nothing for numbers outside the range [0, 999,999,999,999].
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n >= 1000000000000 = Nothing -- Check if the number is out of the valid range
  | n == 0                     = Just "zero"   -- Handle the base case for zero
  | otherwise                  = Just $ convert n -- Convert other valid numbers

-- | Main conversion function that handles chunking and scale words.
convert :: Integer -> String
convert n = intercalate " " $ reverse $ map (\(chunk, scale) -> chunkToEnglish chunk ++ scale) $ filter (\(c, _) -> c /= 0) $ zip (chunks n) scales
  where
    -- Scale words for thousands, millions, billions. Empty string for the units chunk.
    scales = ["", " thousand", " million", " billion"]
    -- Breaks the number into 3-digit chunks, least significant first.
    -- e.g., 12345 -> [345, 12]
    chunks :: Integer -> [Integer]
    chunks 0 = []
    chunks num = (num `mod` 1000) : chunks (num `div` 1000)

-- | Converts a number between 1 and 999 into English words.
-- Assumes input is within this range and non-zero.
chunkToEnglish :: Integer -> String
chunkToEnglish n
  | n < 100   = tensToEnglish n -- Handle numbers less than 100 directly
  | otherwise = let (h, rem) = n `divMod` 100 -- Separate hundreds digit and the remainder
                    hundreds = digitToWord h ++ " hundred" -- Convert hundreds digit
                    remainder = tensToEnglish rem -- Convert the remainder (0-99)
                in if rem == 0
                   then hundreds -- e.g., "two hundred"
                   else hundreds ++ " " ++ remainder -- e.g., "two hundred thirty-four"

-- | Converts a number between 1 and 99 into English words.
-- Assumes input is within this range.
tensToEnglish :: Integer -> String
tensToEnglish n
  | n < 10    = digitToWord n   -- Numbers 1-9
  | n < 20    = teenToWord n    -- Numbers 10-19
  | otherwise = let (t, u) = n `divMod` 10 -- Separate tens digit and units digit
                    tens = tensPrefixToWord t -- Get the word for the tens place (twenty, thirty, etc.)
                in if u == 0
                   then tens -- e.g., "twenty", "thirty"
                   else tens ++ "-" ++ digitToWord u -- e.g., "twenty-one", "thirty-two"

-- | Converts a single digit (1-9) to its English word.
digitToWord :: Integer -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Invalid digit: must be 1-9" -- Should not happen with correct logic

-- | Converts a teen number (10-19) to its English word.
teenToWord :: Integer -> String
teenToWord 10 = "ten"
teenToWord 11 = "eleven"
teenToWord 12 = "twelve"
teenToWord 13 = "thirteen"
teenToWord 14 = "fourteen"
teenToWord 15 = "fifteen"
teenToWord 16 = "sixteen"
teenToWord 17 = "seventeen"
teenToWord 18 = "eighteen"
teenToWord 19 = "nineteen"
teenToWord _ = error "Invalid teen number: must be 10-19" -- Should not happen

-- | Converts a tens digit (2-9) to its corresponding prefix word.
tensPrefixToWord :: Integer -> String
tensPrefixToWord 2 = "twenty"
tensPrefixToWord 3 = "thirty"
tensPrefixToWord 4 = "forty"
tensPrefixToWord 5 = "fifty"
tensPrefixToWord 6 = "sixty"
tensPrefixToWord 7 = "seventy"
tensPrefixToWord 8 = "eighty"
tensPrefixToWord 9 = "ninety"
tensPrefixToWord _ = error "Invalid tens prefix: must be 2-9" -- Should not happen
