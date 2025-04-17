module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)
import Data.List (dropWhileEnd)

-- | Remove leading and trailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Determine whether the input is silence (empty or only whitespace).
isSilence :: String -> Bool
isSilence = null . trim

-- | Determine whether the input is a question (ends with a '?').
isQuestion :: String -> Bool
isQuestion xs = case reverse (trim xs) of
  ('?':_) -> True
  _       -> False

-- | Determine whether the input is yelling:
--   it has at least one alphabetic character and all alphabetic
--   characters are uppercase.
isYelling :: String -> Bool
isYelling xs =
  let letters = filter isAlpha xs
  in not (null letters) && all isUpper letters

-- | Provide Bob's response for a given remark.
responseFor :: String -> String
responseFor xs
  | isSilence xs                 = "Fine. Be that way!"
  | isQuestion xs && isYelling xs = "Calm down, I know what I'm doing!"
  | isYelling xs                 = "Whoa, chill out!"
  | isQuestion xs                = "Sure."
  | otherwise                    = "Whatever."
