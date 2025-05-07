module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLetter)

responseFor :: String -> String
responseFor xs
  | all isSpace xs = "Fine. Be that way!"
  | isYellingQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = dropWhile isSpace . reverse . dropWhile isSpace . reverse $ xs
    isQuestion s = not (null trimmed) && last trimmed == '?'
    isYelling s = any isLetter s && all (\c -> not (isLetter c) || isUpper c) s
    isYellingQuestion s = isYelling s && isQuestion s
