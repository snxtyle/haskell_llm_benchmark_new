module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYellingQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = dropWhile isSpace (reverse (dropWhile isSpace (reverse xs)))
    
    isSilence s = all isSpace s
    
    isQuestion s = not (null trimmed) && last trimmed == '?'
    
    isYelling s = hasLetters trimmed && all isUpper (filter isAlpha trimmed)
    
    isYellingQuestion s = isYelling s && isQuestion s
    
    hasLetters s = any isAlpha s
