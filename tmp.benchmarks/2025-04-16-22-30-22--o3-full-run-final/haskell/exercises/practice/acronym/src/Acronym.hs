module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, toUpper, isSpace)

-- | Convert a phrase to its acronym.
--   Hyphens act like spaces, every other punctuation mark is
--   discarded (but still treated as a separator), and internal
--   CamelCase transitions introduce additional word boundaries.
--
--   >>> abbreviate "As Soon As Possible"
--   "ASAP"
--
--   >>> abbreviate "Liquid-crystal display"
--   "LCD"
--
--   >>> abbreviate "Thank George It's Friday!"
--   "TGIF"
--
--   >>> abbreviate "HyperText Markup Language"
--   "HTML"
abbreviate :: String -> String
abbreviate input =
    [ toUpper c
    | (prev, c) <- zip (' ' : cleaned) cleaned
    , isWordStart prev c
    ]
  where
    -- Pre‑process the string:
    --   * hyphens become spaces (explicit word separators)
    --   * alphabetic characters stay as‑is
    --   * every other character turns into a space so it
    --     behaves like a separator but does not appear in
    --     the output.
    cleaned :: String
    cleaned = map normalise input

    normalise :: Char -> Char
    normalise '-' = ' '
    normalise c
      | isAlpha c = c
      | isSpace c = ' '
      | otherwise = ' '

    -- A character starts a new word (and therefore contributes to
    -- the acronym) when it is alphabetic and either
    --
    --   • the previous character is *not* alphabetic (i.e. we just
    --     crossed a space / punctuation boundary), or
    --   • the previous character is alphabetic but lower‑case
    --     (CamelCase transition, e.g. "HyperText").
    isWordStart :: Char -> Char -> Bool
    isWordStart prev curr =
        isAlpha curr && (not (isAlpha prev) || isLower prev)
