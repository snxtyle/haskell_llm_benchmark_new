module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)

responseFor :: String -> String
responseFor xs
    | isSilence xs = "Fine. Be that way!"
    | isYellingQuestion xs = "Calm down, I know what I'm doing!"
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
isYelling xs = any isAlpha xs && all (\c -> not (isAlpha c) || isUpper c) xs

isYellingQuestion :: String -> Bool
isYellingQuestion xs = isYelling xs && isQuestion xs
