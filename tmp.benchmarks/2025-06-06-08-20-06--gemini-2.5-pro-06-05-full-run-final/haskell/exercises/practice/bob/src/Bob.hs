module Bob (responseFor) where

import Data.Char (isLower, isSpace, isUpper)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilence = "Fine. Be that way!"
  | isYelling && isQuestion = "Calm down, I know what I'm doing!"
  | isYelling = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = dropWhileEnd isSpace xs
    isSilence = null trimmed
    isQuestion = not (null trimmed) && last trimmed == '?'
    isYelling = any isUpper xs && not (any isLower xs)
