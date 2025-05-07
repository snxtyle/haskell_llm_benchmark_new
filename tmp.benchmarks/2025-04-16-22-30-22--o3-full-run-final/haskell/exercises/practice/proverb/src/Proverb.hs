module Proverb (recite) where

import Data.List (intercalate)

-- | Generate the proverb based on the supplied list of words.
--   An empty list yields an empty string.
recite :: [String] -> String
recite [] = ""
recite xs = intercalate "\n" (pairLines ++ [conclusion])
  where
    pairLines   = [pairLine a b | (a, b) <- zip xs (tail xs)]
    pairLine a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    conclusion   = "And all for the want of a " ++ head xs ++ "."
