module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper, isLower)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYellingQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

-- Helper to trim whitespace from both ends
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

isSilence :: String -> Bool
isSilence = null . trim

isQuestion :: String -> Bool
isQuestion s = not (null t) && last t == '?'
  where t = trim s

isYelling :: String -> Bool
isYelling s = any isAlpha t && all (not . isLower) (filter isAlpha t)
  where t = trim s

isYellingQuestion :: String -> Bool
isYellingQuestion s = isYelling s && isQuestion s
