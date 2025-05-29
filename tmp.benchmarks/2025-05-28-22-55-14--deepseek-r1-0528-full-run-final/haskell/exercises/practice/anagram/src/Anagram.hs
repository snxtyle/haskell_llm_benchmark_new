module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates = 
    let normalizedWord = map toLower word
        sortedNormalized = sort normalizedWord
        isAnagram candidate = 
            let normalizedCandidate = map toLower candidate
            in normalizedCandidate /= normalizedWord 
               && sort normalizedCandidate == sortedNormalized
    in filter isAnagram candidates
