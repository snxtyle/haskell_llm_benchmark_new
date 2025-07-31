module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, toUpper)

responseFor :: String -> String
responseFor input
  | isSilence trimmed                 = "Fine. Be that way!"
  | isQuestion trimmed && isYelling s = "Calm down, I know what I'm doing!"
  | isYelling s                       = "Whoa, chill out!"
  | isQuestion trimmed                = "Sure."
  | otherwise                         = "Whatever."
  where
    s = input
    trimmed = trim s

isSilence :: String -> Bool
isSilence = all isSpace

isQuestion :: String -> Bool
isQuestion "" = False
isQuestion xs = last xs == '?'

isYelling :: String -> Bool
isYelling xs =
  let letters = filter isAlpha xs
  in not (null letters) && all (\c -> toUpper c == c) letters

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- dropWhileEnd for base versions without Data.List.dropWhileEnd in scope
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse
