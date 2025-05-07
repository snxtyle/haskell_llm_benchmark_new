module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, isAlphaNum, toLower, ord, chr)
import Data.List (intercalate)

-- | Substitutes a character using the Atbash cipher logic.
-- Returns the original character if it's a digit.
-- Assumes input character is already lowercase if it's a letter.
substituteChar :: Char -> Char
substituteChar c
  | isAlpha c = chr (ord 'a' + ord 'z' - ord c)
  | isDigit c = c
  | otherwise = error "Invalid character passed to substituteChar" -- Should not happen with proper filtering

-- | Groups a list into sublists of a specified size.
groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n xs = take n xs : groupEvery n (drop n xs)

-- | Decodes a string using the Atbash cipher.
decode :: String -> String
decode cipherText = map substituteChar $ filter isAlphaNum cipherText

-- | Encodes a string using the Atbash cipher.
encode :: String -> String
encode plainText =
    let
        -- Convert to lowercase and keep only letters and digits
        cleaned = map toLower $ filter isAlphaNum plainText
        -- Apply Atbash substitution to letters, keep digits
        substituted = map substituteChar cleaned
        -- Group into chunks of 5
        grouped = groupEvery 5 substituted
    in
        -- Join chunks with spaces
        intercalate " " grouped
