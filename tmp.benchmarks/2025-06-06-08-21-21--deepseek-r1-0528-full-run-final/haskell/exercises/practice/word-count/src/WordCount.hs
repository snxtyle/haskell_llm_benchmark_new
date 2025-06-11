module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort, dropWhileEnd)

wordCount :: String -> [(String, Int)]
wordCount input = 
    let lower = map toLower input
        tokens = extractWords lower
    in map (\g@(x:_) -> (x, length g)) . group . sort $ tokens
  where
    allowed c = isAlphaNum c || c == '\''
    extractWords s = case dropWhile (not . allowed) s of
                      [] -> []
                      s' -> let (word, s'') = span allowed s'
                                 cleaned = cleanToken word
                             in if null cleaned 
                                then extractWords s'' 
                                else cleaned : extractWords s''
    cleanToken w = dropWhileEnd (== '\'') (dropWhile (== '\'') w)
