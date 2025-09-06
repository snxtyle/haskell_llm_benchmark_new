module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)

isIsogram :: String -> Bool
isIsogram str = checkUnique [] (map toLower str)
  where
    checkUnique _ [] = True
    checkUnique seen (c:cs)
      | not (isLetter c) = checkUnique seen cs
      | c `elem` seen = False
      | otherwise = checkUnique (c:seen) cs
