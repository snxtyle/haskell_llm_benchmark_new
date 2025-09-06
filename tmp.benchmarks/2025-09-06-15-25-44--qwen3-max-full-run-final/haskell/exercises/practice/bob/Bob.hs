module Bob (responseFor) where

import Data.Char (isLetter, isSpace)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYellingQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isYelling xs = "Whoa, chill out!"
  | otherwise = "Whatever."

-- Check if the input is silence (empty or only whitespace)
isSilence :: String -> Bool
isSilence = all isSpace

-- Check if the input is a question (ends with '?')
isQuestion :: String -> Bool
isQuestion = (=='?') . last . dropWhileEnd isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

-- Check if the input is yelling (all caps with at least one letter)
isYelling :: String -> Bool
isYelling xs = any isLetter xs && all (\c -> not (isLetter c) || isUpperOrNonLetter c) xs
  where
    isUpperOrNonLetter c = not (isLetter c) || (isLetter c && c == toUpper c)
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- Check if the input is a yelling question
isYellingQuestion :: String -> Bool
isYellingQuestion xs = isQuestion xs && isYelling xs
