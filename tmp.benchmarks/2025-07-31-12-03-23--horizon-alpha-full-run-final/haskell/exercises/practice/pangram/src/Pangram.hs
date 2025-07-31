module Pangram (isPangram) where

import Data.Char (toLower, isAsciiLower, isAsciiUpper)
import qualified Data.Set as S

isPangram :: String -> Bool
isPangram text =
  let letters = S.fromList [ toLower c | c <- text, isAsciiLower c || isAsciiUpper c ]
  in S.size letters == 26
