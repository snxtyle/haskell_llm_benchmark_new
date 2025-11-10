module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isAlpha)
import Data.List (isSuffixOf, dropWhileEnd)

-- | Checks if the input is silent (empty or only whitespace).
isSilence :: String -> Bool
isSilence = all isSpace

-- | Checks if the input is yelling (contains at least one letter and all letters are uppercase).
isYelling :: String -> Bool
isYelling xs = any isAlpha xs && all (\c -> not (isAlpha c) || isUpper c) xs

-- | Checks if the input is a question (ends with a question mark, ignoring trailing whitespace).
isQuestion :: String -> Bool
isQuestion xs = isSuffixOf "?" (dropWhileEnd isSpace xs)

-- | Determines Bob's response based on the input string.
responseFor :: String -> String
responseFor xs
  | isSilence xs           = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs           = "Whoa, chill out!"
  | isQuestion xs          = "Sure."
  | otherwise              = "Whatever."
