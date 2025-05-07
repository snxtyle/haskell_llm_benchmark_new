module PigLatin (translate) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord w
  | rule1 w                             = w ++ "ay"
  | Just (pre, suf) <- splitQu w       = suf ++ pre ++ "ay"
  | Just (pre, suf) <- splitY w        = suf ++ pre ++ "ay"
  | otherwise                           = let (con, rest) = span (not . isVowel) w
                                          in rest ++ con ++ "ay"

-- Rule 1: begins with vowel, or "xr"/"yt"
rule1 :: String -> Bool
rule1 (x:y:_) | map toLower [x,y] `elem` ["xr","yt"] = True
rule1 (x:_)   | isVowel x                            = True
rule1 _                                         = False

-- Rule 3: consonant cluster ending in 'q' followed by 'u'
splitQu :: String -> Maybe (String, String)
splitQu w =
  let cons   = takeWhile (not . isVowel) w
      conLen = length cons
  in if conLen > 0 && last cons == 'q'
        && length w > conLen && w !! conLen == 'u'
     then let prefixLen = conLen + 1
              (pre, suf) = splitAt prefixLen w
          in Just (pre, suf)
     else Nothing

-- Rule 4: consonant cluster (treating 'y' as vowel here) followed by 'y'
splitY :: String -> Maybe (String, String)
splitY w =
  let (cons, rest) = span (\c -> not (isVowel c) && c /= 'y') w
  in case rest of
       ('y':_) | not (null cons) -> Just (cons, rest)
       _                         -> Nothing

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"
