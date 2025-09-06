module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)

responseFor :: String -> String
responseFor xs
  | isSilent trimmed = "Fine. Be that way!"
  | isYelling trimmed && isQuestion trimmed = "Calm down, I know what I'm doing!"
  | isYelling trimmed = "Whoa, chill out!"
  | isQuestion trimmed = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = trim xs

isSilent :: String -> Bool
isSilent = all isSpace

isQuestion :: String -> Bool
isQuestion s = not (null s) && last s == '?'

isYelling :: String -> Bool
isYelling s = any isLetter s && all (\c -> not (isLetter c) || isUpper c) s

-- Helper function to trim leading and trailing whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
