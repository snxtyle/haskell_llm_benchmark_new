module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isDigit, toLower, chr, ord)

-- Helper function to perform Atbash substitution on a character.
-- Assumes input char is a lowercase letter or a digit.
transformChar :: Char -> Char
transformChar c
  | isDigit c = c
  | otherwise = chr (ord 'a' + (ord 'z' - ord c)) -- c must be a lowercase letter

-- Helper function to group a list into sublists of a given size.
-- e.g., chunksOf 3 "abcdefg" == ["abc", "def", "g"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

decode :: String -> String
decode cipherText =
    -- 1. Filter: Keep only alphanumeric characters from the ciphertext.
    --    This removes spaces and any other punctuation.
    --    Assumes letters in cipherText are already lowercase as per Atbash standard.
    -- 2. Transform: Apply Atbash substitution to letters, leave digits as is.
    [transformChar char |
     char <- cipherText,
     isAlphaNum char]

encode :: String -> String
encode plainText =
    -- 1. Pre-process: Convert to lowercase and keep only alphanumeric characters.
    let processedChars = [transformChar char | -- 2. Transform: Apply Atbash to letters, leave digits.
                          c <- plainText,
                          let char = toLower c,    -- Convert current char to lowercase
                          isAlphaNum char]       -- Keep only if it's alphanumeric
    -- 3. Group and Format: Join chunks of 5 with spaces.
    --    `unwords` is from Prelude and joins strings with spaces.
    --    If `processedChars` is empty, `chunksOf 5 []` is `[]`, and `unwords []` is `""`.
    in unwords $ chunksOf 5 processedChars
