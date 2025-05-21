module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (sort, group)

-- Helper function to normalize characters:
-- Converts to lowercase, replaces non-alphanumeric/non-apostrophe with space.
normalizeChar :: Char -> Char
normalizeChar c
    | isAlphaNum c || c == '\'' = toLower c
    | otherwise                 = ' '

-- Helper function to strip leading/trailing apostrophes from a word.
-- This handles cases like "'password'" becoming "password" but "that's" remaining "that's".
stripApostrophes :: String -> String
stripApostrophes = dropWhile (=='\'') . reverse . dropWhile (=='\'') . reverse

wordCount :: String -> [(String, Int)]
wordCount xs =
    let
        -- 1. Normalize the input string: convert to lowercase and replace
        --    non-word characters (except internal apostrophes) with spaces.
        normalizedString = map normalizeChar xs

        -- 2. Split the normalized string into tokens by whitespace.
        tokens = words normalizedString

        -- 3. Clean up each token by stripping leading/trailing apostrophes.
        --    Also, filter out any empty strings that might result (e.g., from "''").
        cleanedWords = filter (not . null) $ map stripApostrophes tokens

        -- 4. Sort the words to group identical ones together.
        sortedWords = sort cleanedWords

        -- 5. Group identical words and count their occurrences.
        groupedWords = group sortedWords

        -- 6. Map each group to a (word, count) pair.
        wordCounts = map (\g -> (head g, length g)) groupedWords
    in
        wordCounts
