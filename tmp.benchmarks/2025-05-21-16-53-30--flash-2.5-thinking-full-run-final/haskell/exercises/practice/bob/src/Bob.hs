module Bob (responseFor) where

import Data.Char (isSpace, isLetter, isUpper)
import Data.List (dropWhileEnd)

-- | Bob's response for a given remark.
responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isQuestion xs && isYelling xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

-- | Checks if a string is considered silence (empty or only whitespace).
isSilence :: String -> Bool
isSilence s = null (trim s)

-- | Checks if a string is a question (ends with a question mark after trimming).
isQuestion :: String -> Bool
isQuestion s = not (null trimmed) && last trimmed == '?'
  where trimmed = trim s

-- | Checks if a string is yelling (contains at least one letter and all letters are uppercase).
isYelling :: String -> Bool
isYelling s = any isLetter s && all (\c -> not (isLetter c) || isUpper c) s

-- | Trims leading and trailing whitespace from a string.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
