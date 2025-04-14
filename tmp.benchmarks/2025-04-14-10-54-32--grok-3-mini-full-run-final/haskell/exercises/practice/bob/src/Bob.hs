module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYellingQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isYelling xs = "Whoa, chill out!"
  | otherwise    = "Whatever."

-- Helper function to trim whitespace from a string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Check if the input is silence (empty or only whitespace after trimming)
isSilence :: String -> Bool
isSilence xs = trim xs == ""

-- Check if the input is a question (ends with '?' after trimming)
isQuestion :: String -> Bool
isQuestion xs = not (null trimmed) && last trimmed == '?'
  where trimmed = trim xs

-- Check if the input is yelling (contains letters and all letters are uppercase)
isYelling :: String -> Bool
isYelling xs = 
  let trimmed = trim xs
      hasLetters = any isLetter trimmed
      allUpper = all (\c -> not (isLetter c) || isUpper c) trimmed
  in hasLetters && allUpper

-- Check if the input is a yelling question
isYellingQuestion :: String -> Bool
isYellingQuestion xs = isYelling xs && isQuestion xs
