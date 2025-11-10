module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.Map (Map, fromListWith, toList)

-- | Counts the occurrences of each word in a given text.
-- Words are case-insensitive and can contain apostrophes.
-- Numbers are treated as words.
wordCount :: String -> [(String, Int)]
wordCount text = toList $ countWords $ normalizeWords $ tokenize text
  where
    -- Split the text into raw words based on non-word characters.
    -- A word character is an alphanumeric character or an apostrophe.
    tokenize :: String -> [String]
    tokenize = words . map (\c -> if isWordChar c then c else ' ')
      where
        isWordChar :: Char -> Bool
        isWordChar c = isAlphaNum c || c == '\''

    -- Convert all words to lowercase.
    normalizeWords :: [String] -> [String]
    normalizeWords = map (map toLower)

    -- Count the frequency of each word using a Map.
    countWords :: [String] -> Map String Int
    countWords = fromListWith (+) . map (\w -> (w, 1))
