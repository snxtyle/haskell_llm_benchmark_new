module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

splitWords :: String -> [String]
splitWords s = go s []
  where
    go [] acc = if null acc then [] else [reverse acc]
    go (c:cs) acc
      | isAlphaNum c = go cs (c : acc)
      | c == '\'' =
          if not (null acc) && not (null cs) && isAlphaNum (head cs)
            then go cs (c : acc)
            else (if null acc then id else (reverse acc :)) (go cs [])
      | otherwise = (if null acc then id else (reverse acc :)) (go cs [])

wordCount :: String -> [(String, Int)]
wordCount s = map (\(x:xs) -> (x, 1 + length xs)) . group . sort . map (map toLower) . splitWords $ s
