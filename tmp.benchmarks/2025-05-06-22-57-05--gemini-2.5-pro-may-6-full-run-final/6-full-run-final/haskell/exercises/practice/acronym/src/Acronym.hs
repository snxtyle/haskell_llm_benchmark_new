module Acronym (abbreviate) where

import Data.Char (isLetter, isLower, isSpace, isUpper, toUpper)

-- | Extracts characters from a single word that should form part of an acronym.
-- The first character is always taken.
-- Subsequent uppercase characters are taken if they follow a lowercase character (camelCase).
extractAcroCharsFromWord :: String -> String
extractAcroCharsFromWord "" = ""
extractAcroCharsFromWord (firstChar:restChars) = firstChar : findCamelCaseChars firstChar restChars
  where
    -- Helper to scan the rest of the word for camelCase transitions
    findCamelCaseChars :: Char -> String -> String
    findCamelCaseChars _ "" = ""
    findCamelCaseChars prevChar (currentChar:remainingChars) =
      if isUpper currentChar && isLower prevChar then
        currentChar : findCamelCaseChars currentChar remainingChars
      else
        findCamelCaseChars currentChar remainingChars

-- | Converts a phrase to its acronym.
-- Hyphens are treated as word separators.
-- All other punctuation is removed.
-- For each word (or part of a camelCase word), the significant letter is taken,
-- and all collected letters are uppercased and concatenated.
abbreviate :: String -> String
abbreviate xs = map toUpper $ concatMap extractAcroCharsFromWord $ words $ filter keepLetterOrSpace $ map replaceHyphenWithSpace xs
  where
    -- Replaces hyphens with spaces. Other characters are unchanged.
    replaceHyphenWithSpace :: Char -> Char
    replaceHyphenWithSpace c = if c == '-' then ' ' else c

    -- Checks if a character should be kept (i.e., it's a letter or a space).
    -- This effectively removes punctuation (other than hyphens, which are already spaces).
    keepLetterOrSpace :: Char -> Bool
    keepLetterOrSpace c = isLetter c || isSpace c

-- Example of the function pipeline for "HyperText Markup Language":
-- 1. xs                                     => "HyperText Markup Language"
-- 2. map replaceHyphenWithSpace xs          => "HyperText Markup Language" (no hyphens)
-- 3. filter keepLetterOrSpace ...           => "HyperText Markup Language" (no other punctuation)
-- 4. words ...                              => ["HyperText", "Markup", "Language"]
-- 5. concatMap extractAcroCharsFromWord ...
--    - extractAcroCharsFromWord "HyperText"  => "HT" (H is first, T is Upper after lower r)
--    - extractAcroCharsFromWord "Markup"     => "M"  (M is first)
--    - extractAcroCharsFromWord "Language"   => "L"  (L is first)
--    Result: "HTML"
-- 6. map toUpper ...                        => "HTML" (already uppercase)
