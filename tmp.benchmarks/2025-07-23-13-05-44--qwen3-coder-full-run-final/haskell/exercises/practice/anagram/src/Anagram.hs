module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = 
    let targetLower = map toLower target
        targetSorted = sort targetLower
    in [candidate | candidate <- candidates,
                    let candidateLower = map toLower candidate,
                    candidateLower /= targetLower,
                    sort candidateLower == targetSorted]
