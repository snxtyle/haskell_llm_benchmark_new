module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)

-- | Convert a phrase to its acronym.
--   Words are delimited by whitespace or hyphens.
--   All other punctuation is ignored.
--
--   Examples:
--     abbreviate "Portable Network Graphics" ==> "PNG"
--     abbreviate "Liquid-crystal display"   ==> "LCD"
--     abbreviate "Thank George It's Friday!"==> "TGIF"
--     abbreviate "HyperText Markup Language" ==> "HTML"
abbreviate :: String -> String
abbreviate = map toUpper . concatMap wordInitials . splitOnDelims
  where
    -- Treat hyphens like spaces, then use 'words' to split.
    splitOnDelims :: String -> [String]
    splitOnDelims = words . map replaceDelim
      where
        replaceDelim '-' = ' '
        replaceDelim  c  =  c

    -- Extract all initials for a single token.
    --  1. First alphabetic character in the token (if any).
    --  2. Every uppercase character that is immediately
    --     preceded by a lowercase character (camel-case split).
    wordInitials :: String -> String
    wordInitials token =
      case dropWhile (not . isAlpha) token of
        []        -> []
        (c : cs)  -> c : [ cur
                         | (prev, cur) <- zip (c : cs) cs
                         , isLower prev && isUpper cur
                         ]
