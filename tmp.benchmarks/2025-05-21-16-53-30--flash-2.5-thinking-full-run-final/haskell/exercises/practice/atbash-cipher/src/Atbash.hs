module Atbash (decode, encode) where

import Data.Char (isAsciiLower, isDigit, toLower)

-- | Applies the Atbash transformation to a lowercase ASCII letter.
-- For example, 'a' becomes 'z', 'b' becomes 'y', etc.
-- This function assumes the input character is a lowercase ASCII letter.
atbashChar :: Char -> Char
atbashChar c = toEnum (fromEnum 'a' + fromEnum 'z' - fromEnum c)

-- | Processes a single character for both encoding and decoding.
-- Returns 'Just' the transformed character if it's a lowercase letter or a digit.
-- Returns 'Nothing' if the character should be discarded (e.g., punctuation, spaces, uppercase letters).
processChar :: Char -> Maybe Char
processChar c
  | isAsciiLower c = Just (atbashChar c)
  | isDigit c      = Just c
  | otherwise      = Nothing

-- | Encodes a plain text string using the Atbash cipher.
-- Converts input to lowercase, filters non-alphanumeric characters,
-- applies the Atbash transformation, and groups the output into 5-character chunks.
encode :: String -> String
encode plainText =
  let
    -- Normalize input: convert to lowercase, filter out non-alphanumeric,
    -- and apply Atbash transform to letters.
    transformedChars =
      [ c'
      | c <- map toLower plainText
      , Just c' <- [processChar c]
      ]

    -- Helper to group a list into chunks of a specified size.
    -- This is a manual implementation as Data.List.Split is not a standard library.
    groupIntoChunks :: Int -> String -> [String]
    groupIntoChunks _ [] = []
    groupIntoChunks n xs = take n xs : groupIntoChunks n (drop n xs)
  in
    -- Join the 5-character chunks with spaces.
    unwords (groupIntoChunks 5 transformedChars)

-- | Decodes a cipher text string using the Atbash cipher.
-- Filters out non-alphanumeric characters (including spaces from grouping)
-- and applies the Atbash transformation.
decode :: String -> String
decode cipherText =
  let
    -- Normalize input: convert to lowercase (defensive), filter out non-alphanumeric,
    -- and apply Atbash transform. Since Atbash is its own inverse, processChar works.
    transformedChars =
      [ c'
      | c <- map toLower cipherText
      , Just c' <- [processChar c]
      ]
  in
    transformedChars
