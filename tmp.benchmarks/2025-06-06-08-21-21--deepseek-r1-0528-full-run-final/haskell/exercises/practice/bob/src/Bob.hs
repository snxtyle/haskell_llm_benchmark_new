module Bob (responseFor) where

import Data.Char (isSpace, isLetter, toUpper)

responseFor :: String -> String
responseFor s
  | silence = "Fine. Be that way!"
  | yelling && question = "Calm down, I know what I'm doing!"
  | question = "Sure."
  | yelling = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    trimmed = reverse (dropWhile isSpace (reverse s))
    silence = all isSpace s
    question = not (null trimmed) && last trimmed == '?'
    yelling = any isLetter s && map toUpper s == s
