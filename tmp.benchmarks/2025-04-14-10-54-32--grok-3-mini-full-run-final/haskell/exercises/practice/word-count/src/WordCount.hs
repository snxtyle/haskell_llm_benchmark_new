module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort, words)

wordCount :: String -> [(String, Int)]
wordCount xs = 
  let lowerStr = map toLower xs  -- Convert to lowercase
      processedStr = map (\c -> if isAlphaNum c || c == '\'' then c else ' ') lowerStr  -- Replace non-alphanumeric and non-apostrophe characters with spaces
      tokens = words processedStr  -- Split into tokens
      filteredTokens = filter (any isAlphaNum) tokens  -- Filter out tokens without alphanumeric characters
      strippedTokens = map stripQuotes filteredTokens  -- Strip leading and trailing quotes
      sortedTokens = sort strippedTokens  -- Sort the tokens
      grouped = group sortedTokens  -- Group identical tokens
      counts = map (\grp -> (head grp, length grp)) grouped  -- Create list of (word, count)
  in counts
  where
    stripQuotes :: String -> String
    stripQuotes s = 
      let frontDropped = dropWhile (`elem` ['\'', '"']) s  -- Drop leading ' or "
          backDropped = reverse (dropWhile (`elem` ['\'', '"']) (reverse frontDropped))  -- Drop trailing ' or "
      in backDropped
