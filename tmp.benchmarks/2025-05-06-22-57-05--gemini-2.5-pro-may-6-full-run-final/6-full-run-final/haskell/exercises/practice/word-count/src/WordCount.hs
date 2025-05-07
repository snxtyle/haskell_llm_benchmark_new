module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (sort, group, dropWhileEnd)

wordCount :: String -> [(String, Int)]
wordCount xs = counts
  where
    -- Step 1: Normalize characters
    -- Convert to lowercase, keep apostrophes, map other punctuation/whitespace to space.
    normalizeChar :: Char -> Char
    normalizeChar c
        | isAlphaNum c = toLower c
        | c == '\''    = '\''         -- Keep apostrophes for now
        | otherwise    = ' '          -- Treat all other characters as spaces

    normalizedString :: String
    normalizedString = map normalizeChar xs

    -- Step 2: Split into candidate words
    -- `words` splits the string by spaces, handling multiple spaces correctly.
    candidateWords :: [String]
    candidateWords = words normalizedString

    -- Step 3: Clean candidate words
    -- Remove leading/trailing apostrophes that are likely from quotes,
    -- e.g., "'word'" becomes "word", but "it's" remains "it's".
    -- `dropWhile (== '\'')` removes leading apostrophes.
    -- `dropWhileEnd (== '\'')` removes trailing apostrophes.
    cleanWord :: String -> String
    cleanWord w = dropWhileEnd (== '\'') $ dropWhile (== '\'') w

    cleanedWords :: [String]
    cleanedWords = map cleanWord candidateWords

    -- Step 4: Filter out empty words
    -- Cleaning might result in empty strings (e.g., if a word was "''").
    actualWords :: [String]
    actualWords = filter (not . null) cleanedWords

    -- Step 5: Sort words to prepare for grouping
    sortedWords :: [String]
    sortedWords = sort actualWords

    -- Step 6: Group identical words
    -- `group` takes a sorted list and groups equal adjacent elements.
    groupedWords :: [[String]]
    groupedWords = group sortedWords

    -- Step 7: Map groups to (word, count) tuples
    -- For each group (list of identical words), take the head as the word
    -- and the length of the group as its count.
    counts :: [(String, Int)]
    counts = map (\g -> (head g, length g)) groupedWords
