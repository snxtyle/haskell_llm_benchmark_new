module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLower, isLetter)
import Data.List (dropWhileEnd)

-- Helper function to trim leading and trailing whitespace
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Helper function to check if the string contains any letters
containsLetter :: String -> Bool
containsLetter = any isLetter

-- Helper function to check if all letters in the string are uppercase
allLettersUpper :: String -> Bool
allLettersUpper s = not (any isLower s)

-- Helper function to check if the string is yelled
-- It must contain letters, and all letters must be uppercase.
isYelling :: String -> Bool
isYelling s = containsLetter s && allLettersUpper s

-- Helper function to check if the string is a question (ends with '?')
-- Operates on the trimmed string.
isQuestion :: String -> Bool
isQuestion s = not (null s) && last s == '?'

-- Helper function to check if the string represents silence
-- Operates on the trimmed string.
isSilence :: String -> Bool
isSilence = null

responseFor :: String -> String
responseFor xs
    | isSilence trimmedXs = "Fine. Be that way!"
    | isYelling xs && isQuestion trimmedXs = "Calm down, I know what I'm doing!"
    | isYelling xs = "Whoa, chill out!"
    | isQuestion trimmedXs = "Sure."
    | otherwise = "Whatever."
    where
        trimmedXs = trim xs
