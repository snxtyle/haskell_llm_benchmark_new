module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = 
    filter (isAnagram target) candidates
    where
        isAnagram :: String -> String -> Bool
        isAnagram word candidate
            | lowerWord == lowerCandidate = False
            | otherwise = sortedLowerWord == sortedLowerCandidate
            where
                lowerWord = map toLower word
                lowerCandidate = map toLower candidate
                sortedLowerWord = sort lowerWord
                sortedLowerCandidate = sort lowerCandidate
