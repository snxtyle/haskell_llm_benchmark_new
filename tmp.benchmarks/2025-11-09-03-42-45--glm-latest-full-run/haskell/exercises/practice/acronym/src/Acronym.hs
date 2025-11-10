module Acronym (abbreviate) where

import Data.Char

-- | Convert a phrase to its acronym.
--
-- Punctuation is handled as follows: hyphens are word separators
-- (like whitespace); all other punctuation can be removed from the input.
--
-- >>> abbreviate "Portable Network Graphics"
-- "PNG"
-- >>> abbreviate "Ruby on Rails"
-- "ROR"
-- >>> abbreviate "First-In, First-Out"
-- "FIFO"
-- >>> abbreviate "HyperText Markup Language"
-- "HTML"
abbreviate :: String -> String
abbreviate xs = reverse $ go cleaned ' ' []
  where
    -- Replace hyphens with spaces and remove non-letter/space characters
    cleaned = filter (\c -> isLetter c || isSpace c) $ map (\c -> if c == '-' then ' ' else c) xs

    -- Recursive helper function to build the acronym
    -- It takes the remaining string, the previous character, and the accumulator
    go :: String -> Char -> String -> String
    go [] _ acc = acc
    go (c:cs) prev acc
        -- A character starts a word if the previous char was a space,
        -- or if there was a transition from lower to upper case.
        -- The initial 'prev' is a space, so the first letter is always taken.
        | isSpace prev || (isLower prev && isUpper c) = go cs c (toUpper c : acc)
        | otherwise = go cs c acc
