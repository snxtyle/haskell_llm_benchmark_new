module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\ys -> normalize ys == normalize xs && not (caseInsensitiveEqual ys xs)) xss
  where
    normalize s = sort (map toLower s)
    caseInsensitiveEqual a b = map toLower a == map toLower b
