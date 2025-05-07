module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isLower)
import Data.List (dropWhileEnd)

-- Checks if the input string consists only of whitespace characters or is empty.
isSilence :: String -> Bool
isSilence = all isSpace

-- Checks if the input string is yelled.
-- A string is yelled if it contains at least one alphabetic character,
-- and all alphabetic characters in it are uppercase (i.e., no lowercase letters).
isYelling :: String -> Bool
isYelling xs = any isAlpha xs && not (any isLower xs)

-- Checks if the input string is a question.
-- A question is defined as a string that ends with a '?'
-- after any trailing whitespace characters are removed.
isQuestion :: String -> Bool
isQuestion xs = case dropWhileEnd isSpace xs of
    ""    -> False -- An empty string or string of only spaces is not a question
    ys    -> last ys == '?'

responseFor :: String -> String
responseFor xs
    -- Rule: "Fine. Be that way!" for silence.
    | isSilence xs                                      = "Fine. Be that way!"
    -- Rule: "Calm down, I know what I'm doing!" for a yelled question.
    -- This must be checked before general yelling or general question.
    | isYelling xs && isQuestion xs                     = "Calm down, I know what I'm doing!"
    -- Rule: "Whoa, chill out!" for yelling.
    | isYelling xs                                      = "Whoa, chill out!"
    -- Rule: "Sure." for a question.
    | isQuestion xs                                     = "Sure."
    -- Rule: "Whatever." for anything else.
    | otherwise                                         = "Whatever."
