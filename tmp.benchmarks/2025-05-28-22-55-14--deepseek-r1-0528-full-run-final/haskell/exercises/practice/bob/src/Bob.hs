module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isYelling xs = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isSilence = all isSpace

    isQuestion s = 
        let rev = reverse s
            revWithoutTrailing = dropWhile isSpace rev
        in case revWithoutTrailing of
             ('?':_) -> True
             _ -> False

    isYelling s = 
        let letters = filter isAlpha s
        in not (null letters) && all isUpper letters
