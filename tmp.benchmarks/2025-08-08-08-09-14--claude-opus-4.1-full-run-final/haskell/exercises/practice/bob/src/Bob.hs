module Bob (responseFor) where

import Data.Char (isSpace, isLetter, isUpper)

responseFor :: String -> String
responseFor xs
    | isSilence xs = "Fine. Be that way!"
    | isYelledQuestion xs = "Calm down, I know what I'm doing!"
    | isQuestion xs = "Sure."
    | isYelling xs = "Whoa, chill out!"
    | otherwise = "Whatever."

isSilence :: String -> Bool
isSilence = all isSpace

isQuestion :: String -> Bool
isQuestion xs = case dropWhile isSpace (reverse xs) of
    ('?':_) -> True
    _ -> False

isYelling :: String -> Bool
isYelling xs = hasLetters xs && all isUpper (filter isLetter xs)
    where hasLetters = any isLetter

isYelledQuestion :: String -> Bool
isYelledQuestion xs = isQuestion xs && isYelling xs
