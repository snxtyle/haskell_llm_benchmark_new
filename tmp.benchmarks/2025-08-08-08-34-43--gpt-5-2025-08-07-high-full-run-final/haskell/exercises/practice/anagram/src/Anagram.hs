module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates = filter isAnagram candidates
  where
    lowerWord = map toLower word
    sigWord = sort lowerWord
    isAnagram candidate =
      let lowerCand = map toLower candidate
      in lowerCand /= lowerWord && sort lowerCand == sigWord
