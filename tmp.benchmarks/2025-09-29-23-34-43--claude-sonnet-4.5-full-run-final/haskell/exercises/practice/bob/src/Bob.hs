module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLetter)

responseFor :: String -> String
responseFor xs
  | isSilence xs                = "Fine. Be that way!"
  | isYellingQuestion xs        = "Calm down, I know what I'm doing!"
  | isYelling xs                = "Whoa, chill out!"
  | isQuestion xs               = "Sure."
  | otherwise                   = "Whatever."

isSilence :: String -> Bool
isSilence = all isSpace

isQuestion :: String -> Bool
isQuestion xs = case dropWhile isSpace (reverse xs) of
  ('?':_) -> True
  _       -> False

isYelling :: String -> Bool
isYelling xs = hasLetters xs && all isUpper (filter isLetter xs)

isYellingQuestion :: String -> Bool
isYellingQuestion xs = isYelling xs && isQuestion xs

hasLetters :: String -> Bool
hasLetters = any isLetter
