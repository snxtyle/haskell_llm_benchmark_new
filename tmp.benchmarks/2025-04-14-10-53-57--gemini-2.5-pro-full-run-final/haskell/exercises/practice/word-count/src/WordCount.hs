module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort, dropWhileEnd)

-- Helper function to normalize the input string for word splitting.
-- Converts characters that are not alphanumeric and not an apostrophe to spaces.
normalize :: String -> String
normalize = map normalizeChar
  where
    normalizeChar c
      -- Keep alphanumeric characters and apostrophes
      | isAlphaNum c || c == '\'' = c
      -- Replace everything else with a space
      | otherwise = ' '

-- Helper function to trim leading and trailing apostrophes from a string.
trimApostrophes :: String -> String
-- dropWhileEnd requires base >= 4.5.0.0 (GHC >= 7.4.1)
trimApostrophes = dropWhileEnd (== '\'') . dropWhile (== '\'')

wordCount :: String -> [(String, Int)]
wordCount xs =
    let -- Convert input to lowercase for case-insensitivity
        lowerXs = map toLower xs
        -- Replace non-word characters (excluding apostrophes) with spaces
        normalizedXs = normalize lowerXs
        -- Split the string into potential words based on spaces
        potentialWords = words normalizedXs
        -- Trim leading/trailing apostrophes from each potential word
        trimmedWords = map trimApostrophes potentialWords
        -- Filter out any empty strings that resulted from trimming
        validWords = filter (not . null) trimmedWords
        -- Sort words to prepare for grouping
        sortedWords = sort validWords
        -- Group identical words together
        groupedWords = group sortedWords
        -- Create (word, count) tuples from the groups
        counts = map (\ws -> (head ws, length ws)) groupedWords
    in counts
