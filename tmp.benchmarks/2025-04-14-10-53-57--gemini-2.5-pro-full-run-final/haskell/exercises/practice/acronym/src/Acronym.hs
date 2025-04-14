module Acronym (abbreviate) where

import Data.Char (isLetter, isUpper, isLower, toUpper)

-- | Converts a phrase to its acronym.
-- Hyphens are treated as word separators.
-- CamelCase words contribute letters to the acronym.
-- All other punctuation is removed.
--
-- Examples:
--   abbreviate "Portable Network Graphics" == "PNG"
--   abbreviate "Ruby on Rails" == "ROR"
--   abbreviate "First In, First Out" == "FIFO"
--   abbreviate "GNU Image Manipulation Program" == "GIMP"
--   abbreviate "Complementary metal-oxide semiconductor" == "CMOS"
--   abbreviate "Liquid-crystal display" == "LCD"
--   abbreviate "Thank George It's Friday!" == "TGIF"
--   abbreviate "HyperText Markup Language" == "HTML"
abbreviate :: String -> String
abbreviate xs = go ' ' xs -- Use a space as the initial 'previous' character
  where
    -- Helper function: processes the string recursively
    -- Takes the previous character and the remaining string
    -- Returns the acronym part derived from the remaining string
    go :: Char -> String -> String
    go _ [] = "" -- Base case: empty string yields empty acronym
    go prev (c:cs)
        -- Check if the current character 'c' should start an acronym component
      | isAcronymStart prev c && isLetter c = toUpper c : go c cs -- If yes, add its uppercase version and recurse
      | otherwise                           = go c cs             -- If no, just recurse, updating the previous char

    -- Determines if a character 'cur' should start an acronym component,
    -- based on the 'prev' character.
    isAcronymStart :: Char -> Char -> Bool
    isAcronymStart prev cur =
         isSeparator prev           -- Starts after a separator (space or hyphen)
      || (isLower prev && isUpper cur) -- Starts due to camelCase (lower followed by upper)

    -- Checks if a character is a word separator
    isSeparator :: Char -> Bool
    isSeparator c = c == ' ' || c == '-'
