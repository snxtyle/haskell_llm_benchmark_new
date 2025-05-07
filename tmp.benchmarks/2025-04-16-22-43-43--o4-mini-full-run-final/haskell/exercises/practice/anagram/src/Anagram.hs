module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    normalize = sort . map toLower
    targetNorm = normalize xs
    origLower  = map toLower xs
    isAnagram w = let wNorm = normalize w
                      wLower = map toLower w
                  in wNorm == targetNorm && wLower /= origLower
