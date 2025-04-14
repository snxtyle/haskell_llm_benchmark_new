module PigLatin (translate) where

import Data.Char (toLower)

translate :: String -> String
translate = unwords . map translateWord . words

vowels :: [Char]
vowels = "aeiou"

startsWithVowelOrSpecial :: String -> Bool
startsWithVowelOrSpecial w =
  case map toLower w of
    (x:y:_) -> x `elem` vowels || [x,y] == "xr" || [x,y] == "yt"
    (x:_)   -> x `elem` vowels
    _       -> False

-- Find the index where the first vowel or 'y' (if after consonants) occurs,
-- taking into account the special "qu" cluster after consonants.
-- 'y' is treated as a vowel only if it is not the first letter.
consonantClusterEnd :: String -> Int
consonantClusterEnd w = go 0 w
  where
    go i (x:y:rest)
      | map toLower [x,y] == "qu" = i + 2
      | x `elem` vowels = i
      | x == 'y' && i /= 0 = i
      | otherwise = go (i+1) (y:rest)
    go i (x:xs)
      | x `elem` vowels = i
      | x == 'y' && i /= 0 = i
      | otherwise = go (i+1) xs
    go i [] = i

translateWord :: String -> String
translateWord w
  | startsWithVowelOrSpecial w = w ++ "ay"
  | otherwise =
      let i = consonantClusterEnd w
          (start, rest) = splitAt i w
      in rest ++ start ++ "ay"
