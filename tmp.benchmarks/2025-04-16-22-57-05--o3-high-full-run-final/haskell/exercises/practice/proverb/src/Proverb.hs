module Proverb (recite) where

import Data.List (intercalate)

-- | Produce a proverb from a list of words.
--
--   Each adjacent pair in the input creates a line of the form
--   "For want of a X the Y was lost."
--   The proverb concludes with
--   "And all for the want of a Z."
--   where Z is the first element of the list.
--
--   If the input list is empty, the result is the empty string.
recite :: [String] -> String
recite [] = ""
recite xs = intercalate "\n" (body ++ [conclusion])
  where
    body = [ "For want of a " ++ a ++ " the " ++ b ++ " was lost."
           | (a, b) <- zip xs (tail xs)
           ]
    conclusion = "And all for the want of a " ++ head xs ++ "."
