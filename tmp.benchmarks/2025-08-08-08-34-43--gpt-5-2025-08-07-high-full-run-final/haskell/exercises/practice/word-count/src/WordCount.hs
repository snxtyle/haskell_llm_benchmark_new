module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort, dropWhileEnd)

wordCount :: String -> [(String, Int)]
wordCount xs =
  let normalized = map (\c -> if isAlphaNum c || c == '\'' then toLower c else ' ') xs
      tokens = words normalized
      cleaned = map stripApostrophes tokens
      ws = filter (not . null) cleaned
  in map (\g -> (head g, length g)) . group . sort $ ws

stripApostrophes :: String -> String
stripApostrophes = dropWhileEnd (== '\'') . dropWhile (== '\'')
