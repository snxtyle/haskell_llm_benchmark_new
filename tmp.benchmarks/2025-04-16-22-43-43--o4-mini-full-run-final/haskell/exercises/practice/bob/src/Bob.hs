module Bob (responseFor) where

import Data.Char (isAlpha, isUpper, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilence         = "Fine. Be that way!"
  | isYellingQuestion = "Calm down, I know what I'm doing!"
  | isYelling         = "Whoa, chill out!"
  | isQuestion        = "Sure."
  | otherwise         = "Whatever."
  where
    isSilence = all isSpace xs

    letters = filter isAlpha xs
    isYelling = not (null letters) && all isUpper letters

    trimmed = dropWhileEnd isSpace xs
    isQuestion = not (null trimmed) && last trimmed == '?'

    isYellingQuestion = isYelling && isQuestion
