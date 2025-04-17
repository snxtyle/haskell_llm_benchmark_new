module Bob (responseFor) where

import Data.Char (isAlpha, isLower, isSpace, isUpper)

-- | Produce Bob's response for a given remark.
responseFor :: String -> String
responseFor input
  | isSilence              = "Fine. Be that way!"
  | isQuestion && isYelling = "Calm down, I know what I'm doing!"
  | isQuestion             = "Sure."
  | isYelling              = "Whoa, chill out!"
  | otherwise              = "Whatever."
  where
    trimmed    = trim input
    isSilence  = null trimmed
    isQuestion = not (null trimmed) && last trimmed == '?'

    letters    = filter isAlpha trimmed
    hasUpper   = any isUpper letters
    hasLower   = any isLower letters
    isYelling  = not (null letters) && hasUpper && not hasLower

-- | Remove leading and trailing whitespace from a String.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse
