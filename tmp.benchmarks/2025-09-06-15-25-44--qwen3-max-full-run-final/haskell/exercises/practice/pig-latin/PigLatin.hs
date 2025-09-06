module PigLatin (translate) where

import Data.Char (toLower)
import Data.List (words, unwords)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
  | startsWithVowel word || startsWithXR word || startsWithYT word = word ++ "ay"
  | otherwise = moveConsonantsAndAddAy word

-- Check if word starts with a vowel
startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:_) = toLower x `elem` "aeiou"

-- Check if word starts with "xr"
startsWithXR :: String -> Bool
startsWithXR word = take 2 word == "xr"

-- Check if word starts with "yt"
startsWithYT :: String -> Bool
startsWithYT word = take 2 word == "yt"

-- Move consonants to end and add "ay"
moveConsonantsAndAddAy :: String -> String
moveConsonantsAndAddAy word = case findSplitPoint word 0 of
  0 -> word ++ "ay"  -- This shouldn't happen with the guards above
  n -> drop n word ++ take n word ++ "ay"

-- Find the split point according to Pig Latin rules
findSplitPoint :: String -> Int -> Int
findSplitPoint [] n = n
findSplitPoint (x:xs) n
  | n == 0 && toLower x `elem` "aeiou" = n  -- Rule 1: starts with vowel
  | n >= 0 && n + 1 < length (x:xs) && toLower x == 'q' && toLower (head xs) == 'u' = n + 2  -- Rule 3: "qu"
  | n > 0 && toLower x == 'y' = n  -- Rule 4: consonant(s) followed by 'y'
  | n == 0 && (x == 'x' || x == 'y') && not (null xs) && toLower (head xs) == 'r' = 0  -- "xr"
  | n == 0 && x == 'y' && not (null xs) && toLower (head xs) == 't' = 0  -- "yt"
  | toLower x `elem` "aeiou" = n  -- Found a vowel
  | otherwise = findSplitPoint xs (n + 1)
