module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (dropWhileEnd, group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs =
  let
    -- Normalize the input string:
    -- 1. Convert to lowercase.
    -- 2. Replace non-word characters (anything not alphanumeric or an apostrophe) with spaces.
    normalized = map (\c -> if isAlphaNum c || c == '\'' then c else ' ') (map toLower xs)

    -- Split the normalized string into words.
    allWords = words normalized

    -- Trim leading and trailing apostrophes from each word.
    -- This handles cases like "'word'" which should be counted as "word",
    -- while preserving apostrophes in contractions like "don't".
    trimApostrophes = dropWhile (== '\'') . dropWhileEnd (== '\'')
    trimmedWords = map trimApostrophes allWords

    -- Filter out any empty strings that might result from trimming (e.g. a word that was just "'").
    nonEmptyWords = filter (not . null) trimmedWords

    -- Count word occurrences:
    -- 1. Sort the words to bring identical words together.
    -- 2. Group identical words.
    -- 3. Map each group to a (word, count) tuple.
    wordGroups = group (sort nonEmptyWords)
  in
    map (\ws -> (head ws, length ws)) wordGroups
