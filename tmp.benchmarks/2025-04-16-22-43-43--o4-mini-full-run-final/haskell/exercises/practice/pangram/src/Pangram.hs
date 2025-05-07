module Pangram (isPangram) where

import Data.Char (toLower)
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram text =
  let letters = Set.fromList [ c
                             | c <- map toLower text
                             , c >= 'a' && c <= 'z'
                             ]
  in Set.size letters == 26
