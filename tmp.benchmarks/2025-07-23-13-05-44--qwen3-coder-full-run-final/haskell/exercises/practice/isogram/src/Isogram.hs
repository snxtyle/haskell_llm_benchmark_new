module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)

isIsogram :: String -> Bool
isIsogram phrase = checkIsogram (map toLower (filter isLetter phrase)) []
  where
    checkIsogram [] _ = True
    checkIsogram (x:xs) seen
      | x `elem` seen = False
      | otherwise = checkIsogram xs (x:seen)
