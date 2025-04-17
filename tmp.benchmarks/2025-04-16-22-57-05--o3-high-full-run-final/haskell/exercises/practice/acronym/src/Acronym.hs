module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)

-- | Convert a phrase to its acronym.
--   • Hyphens are treated as word separators (like whitespace).
--   • All other punctuation is removed.
--   • A new word is also considered to start at the transition from a
--     lowercase letter to an uppercase letter within the same token
--     (to support CamelCase inputs).
abbreviate :: String -> String
abbreviate =
    map toUpper               -- produce the final acronym in uppercase
  . concatMap acronymChars    -- obtain the relevant letters from every word
  . words                     -- split on whitespace (hyphens → spaces)
  . preprocess                -- normalise the input
  where
    -- Replace hyphens with spaces and remove every non‑alphabetic
    -- character (except whitespace, which we preserve).
    preprocess :: String -> String
    preprocess [] = []
    preprocess (c:cs)
      | c == '-'                 = ' ' : preprocess cs
      | isAlpha c || c == ' '    =  c  : preprocess cs
      | otherwise                =       preprocess cs

    -- For a given word, take its first letter plus every uppercase letter
    -- that directly follows a lowercase one (handles CamelCase).
    acronymChars :: String -> String
    acronymChars []       = []
    acronymChars (x:xs)   = x : [ c | (p, c) <- zip (x:xs) xs
                                    , isLower p && isUpper c ]
