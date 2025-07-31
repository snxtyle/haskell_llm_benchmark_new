module Bob (responseFor) where

import Data.Char (isAlpha, toUpper)

responseFor :: String -> String
responseFor xs
  | isSilence = "Fine. Be that way!"
  | isYelling && isQuestion = "Calm down, I know what I'm doing!"
  | isYelling = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = trim xs
    isSilence = all (== ' ') trimmed || null trimmed
    isYelling = any isAlpha xs && xs == map toUpper xs
    isQuestion = not (null trimmed) && last trimmed == '?'
    trim = unwords . words
