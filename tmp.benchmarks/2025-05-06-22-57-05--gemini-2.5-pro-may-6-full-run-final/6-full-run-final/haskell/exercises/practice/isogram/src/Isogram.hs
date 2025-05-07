module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram phrase =
    -- Convert the phrase to lowercase, then filter out any characters that are not letters.
    -- This ensures case-insensitivity and that only letters are checked for repetition,
    -- as spaces and hyphens are allowed to appear multiple times.
    let lettersOnly = filter isLetter (map toLower phrase)
    in
    -- An isogram has no repeating letters.
    -- If the length of the list of letters is the same as the length of the list
    -- after removing duplicates (using nub), then all letters were unique.
    length lettersOnly == length (nub lettersOnly)
