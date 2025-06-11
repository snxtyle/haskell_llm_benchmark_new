module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
    let lowerXs = map toLower xs
        sortedXs = sort lowerXs
        isAnagram candidate =
            let lowerCandidate = map toLower candidate
            in lowerXs /= lowerCandidate && sortedXs == sort lowerCandidate
    in filter isAnagram xss
