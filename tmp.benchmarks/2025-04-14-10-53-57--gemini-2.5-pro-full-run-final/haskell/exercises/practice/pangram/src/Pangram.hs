module Pangram (isPangram) where

import Data.Char (toLower, isAsciiLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text =
  let -- Convert the entire string to lowercase
      lowerText = map toLower text
      -- Filter out characters that are not ASCII lowercase letters
      letters = filter isAsciiLower lowerText
      -- Get the unique letters present in the text
      uniqueLetters = nub letters
      -- Define the target alphabet
      alphabet = ['a'..'z']
  -- Check if every letter in the alphabet is present in the unique letters from the text
  in all (`elem` uniqueLetters) alphabet
