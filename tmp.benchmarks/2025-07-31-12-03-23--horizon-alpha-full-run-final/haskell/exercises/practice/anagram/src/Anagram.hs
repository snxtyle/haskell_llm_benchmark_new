module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagramCandidate xss
  where
    norm = map toLower
    target = norm xs
    signature = sort target

    isAnagramCandidate w =
      let lw = norm w
      in lw /= target && sort lw == signature
