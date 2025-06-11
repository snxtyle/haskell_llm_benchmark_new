module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (dropWhileEnd)

-- | Produce Bob's response for a given remark.
responseFor :: String -> String
responseFor xs
  | isSilent trimmed          = "Fine. Be that way!"
  | isYelling trimmed
    && isQuestion trimmed     = "Calm down, I know what I'm doing!"
  | isYelling trimmed         = "Whoa, chill out!"
  | isQuestion trimmed        = "Sure."
  | otherwise                 = "Whatever."
  where
    trimmed = trim xs

-- Helpers --------------------------------------------------------------------

-- | Remove leading and trailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Check if the remark is a question (ends with a '?').
isQuestion :: String -> Bool
isQuestion s = not (null s) && last s == '?'

-- | Check if the remark is silence (empty after trimming).
isSilent :: String -> Bool
isSilent = null

-- | Check if the remark is yelling:
--   it has at least one alphabetic character
--   and all alphabetic characters are uppercase.
isYelling :: String -> Bool
isYelling s = hasLetters && all isUpper letters
  where
    letters     = filter isAlpha s
    hasLetters  = not (null letters)
