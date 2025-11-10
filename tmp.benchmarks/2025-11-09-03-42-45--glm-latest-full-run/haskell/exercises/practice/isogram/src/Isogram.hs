module Isogram (isIsogram) where

import qualified Data.Set as Set
import Data.Char (toLower, isLetter)

isIsogram :: String -> Bool
isIsogram s = go (normalize s) Set.empty
  where
    -- Convert to lowercase and keep only letters
    normalize :: String -> String
    normalize = map toLower . filter isLetter

    -- Recursively check for duplicates using a Set for efficiency
    go :: String -> Set.Set Char -> Bool
    go [] _ = True
    go (x:xs) seen
      | x `Set.member` seen = False
      | otherwise           = go xs (Set.insert x seen)
