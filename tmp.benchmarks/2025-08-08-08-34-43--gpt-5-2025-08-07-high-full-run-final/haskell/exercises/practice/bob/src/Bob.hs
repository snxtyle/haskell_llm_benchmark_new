module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)

responseFor :: String -> String
responseFor xs
  | isSilence t                   = "Fine. Be that way!"
  | isQuestion t && isYelling t   = "Calm down, I know what I'm doing!"
  | isYelling t                   = "Whoa, chill out!"
  | isQuestion t                  = "Sure."
  | otherwise                     = "Whatever."
  where
    t = trim xs

isSilence :: String -> Bool
isSilence = null

isQuestion :: String -> Bool
isQuestion s = not (null s) && last s == '?'

isYelling :: String -> Bool
isYelling s =
  let letters = filter isAlpha s
  in not (null letters) && all isUpper letters

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
