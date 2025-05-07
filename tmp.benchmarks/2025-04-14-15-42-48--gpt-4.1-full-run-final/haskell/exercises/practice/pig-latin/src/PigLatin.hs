module PigLatin (translate) where

import Data.Char (toLower, isAlpha)
import Data.List (isPrefixOf)

-- Helper to check if a character is a vowel
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

-- Helper to check if a word starts with a vowel sound (including "xr" and "yt")
startsWithVowelSound :: String -> Bool
startsWithVowelSound w =
    case map toLower w of
        ('x':'r':_) -> True
        ('y':'t':_) -> True
        (c:_)       -> isVowel c
        _           -> False

-- Helper to find the index where the first vowel sound occurs, considering "qu" and "y"
firstVowelIndex :: String -> Int
firstVowelIndex w = go 0 w
  where
    go i [] = i
    go i ('q':'u':xs) = i + 2 -- treat "qu" as a unit
    go i (c:cs)
      | isVowel c = i
      | c == 'y' && i /= 0 = i -- "y" as a vowel, but not at the start
      | otherwise = go (i+1) cs

-- Translate a single word to Pig Latin
translateWord :: String -> String
translateWord w
    | startsWithVowelSound w = w ++ "ay"
    | otherwise =
        let idx = firstVowelIndex w
            (start, end) = splitAt idx w
        in end ++ start ++ "ay"

-- Split a string into words, preserving non-alpha as separators
splitWords :: String -> [String]
splitWords [] = []
splitWords s@(c:_)
    | isAlpha c =
        let (w, rest) = span isAlpha s
        in w : splitWords rest
    | otherwise =
        let (sep, rest) = span (not . isAlpha) s
        in sep : splitWords rest

-- Recombine words, preserving separators
joinWords :: [String] -> String
joinWords = concat

-- Main translate function
translate :: String -> String
translate xs = joinWords $ map f (splitWords xs)
  where
    f w
      | all isAlpha w && not (null w) = translateWord w
      | otherwise = w
