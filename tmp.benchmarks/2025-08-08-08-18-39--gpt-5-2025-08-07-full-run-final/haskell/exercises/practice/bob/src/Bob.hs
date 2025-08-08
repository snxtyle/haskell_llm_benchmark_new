module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isSilence :: String -> Bool
isSilence = all isSpace

isQuestion :: String -> Bool
isQuestion s =
  let t = trimEnd s
  in not (null t) && last t == '?'

isYelling :: String -> Bool
isYelling s =
  let letters = filter isAlpha s
  in not (null letters) && all isUpper letters

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace
