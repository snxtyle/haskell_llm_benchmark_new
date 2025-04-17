module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Given a target word and a list of candidate words, return the subset
--   of candidates that are anagrams of the target (case‑insensitive),
--   excluding the target word itself.
anagramsFor :: String -> [String] -> [String]
anagramsFor target = filter isAnagram
  where
    -- Convert a word to its canonical form: lowercase and letters sorted.
    canonical :: String -> String
    canonical = sort . map toLower

    targetCanonical :: String
    targetCanonical = canonical target

    targetLower :: String
    targetLower = map toLower target

    -- A candidate is an anagram if its canonical form matches the target's
    -- canonical form, but it is not the same word (ignoring case).
    isAnagram :: String -> Bool
    isAnagram w =
      let wLower = map toLower w
       in wLower /= targetLower && canonical w == targetCanonical
