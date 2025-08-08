module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isSpace, isUpper, toUpper)

-- Convert a phrase to its acronym.
-- Rules:
-- - Take the first letter of each word.
-- - Hyphens are word separators (like whitespace).
-- - Other punctuation is ignored/removed.
-- - Within a word, also take an uppercase letter that follows a lowercase letter (camelCase).
abbreviate :: String -> String
abbreviate = go True False
  where
    -- go boundary prevLower rest
    -- boundary: previous character was a separator (start, whitespace, or hyphen)
    -- prevLower: previous character was a lowercase letter
    go :: Bool -> Bool -> String -> String
    go _ _ [] = []
    go boundary prevLower (c:cs)
      | isAlpha c =
          let takeHere = boundary || (isUpper c && prevLower)
              picked   = [toUpper c | takeHere]
          in picked ++ go False (isLower c) cs
      | c == '-' || isSpace c =
          -- Hyphens and whitespace start a new word
          go True False cs
      | otherwise =
          -- Other punctuation is removed; it doesn't create a new word;
          -- importantly, do not clear the boundary so emphasized words like _Not_ work.
          go boundary False cs
