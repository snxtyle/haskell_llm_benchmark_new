module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | all isSpace xs = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."
  where
    trim s = dropWhileEnd isSpace s
    isYelling s = let t = trim s; letters = filter isLetter t in not (null letters) && all isUpper letters
    isQuestion s = let t = trim s in not (null t) && last t == '?'
