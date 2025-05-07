module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Given a target word and a list of candidate words,
-- | this function finds the subset of candidates that are anagrams of the target.
anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates =
    let
        -- Convert the target word to lowercase for case-insensitive comparison.
        lowerTarget :: String
        lowerTarget = map toLower target

        -- Sort the characters of the lowercase target word.
        -- This creates a canonical representation for anagram checking.
        sortedLowerTarget :: String
        sortedLowerTarget = sort lowerTarget

        -- | Predicate function to check if a single candidate word is an anagram of the target.
        isAnagramOfTarget :: String -> Bool
        isAnagramOfTarget candidate =
            let
                -- Convert the candidate word to lowercase.
                lowerCandidate :: String
                lowerCandidate = map toLower candidate
            in
            -- Rule 1: A word is not its own anagram.
            -- This check must be case-insensitive for the words themselves.
            -- e.g., "stop" is not an anagram of "Stop".
            lowerTarget /= lowerCandidate &&

            -- Rule 2: Anagrams must have the same length.
            -- This is implicitly handled by comparing sorted strings,
            -- but an explicit check could be a micro-optimization.
            -- length lowerTarget == length lowerCandidate &&

            -- Rule 3: The sorted lowercase versions of the target and candidate must be identical.
            -- This ensures they are rearrangements of the same letters.
            sortedLowerTarget == sort lowerCandidate

    -- Filter the original list of candidates using the isAnagramOfTarget predicate.
    -- This preserves the original casing of the anagrams found.
    in filter isAnagramOfTarget candidates
