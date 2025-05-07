module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = map (\ws -> (head ws, length ws)) . group . sort $ words'
  where
    -- Convert to lowercase and split into words, keeping apostrophes inside words
    words' = map (map toLower) $ splitWords xs

-- Split the input string into words, where words are sequences of letters, digits, and apostrophes,
-- but apostrophes only allowed inside words (not at start or end).
splitWords :: String -> [String]
splitWords = filter (not . null) . go []
  where
    go acc [] = [reverse acc | not (null acc)]
    go acc (c:cs)
      | isWordChar c = go (c:acc) cs
      | otherwise =
          let w = reverse acc
          in if null w
             then go [] cs
             else w : go [] cs

    isWordChar c = isAlphaNum c || c == '\''
