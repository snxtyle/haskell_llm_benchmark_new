module PigLatin (translate) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

-- | Translate an English sentence to Pig Latin.
--
--   The implementation follows these rules:
--   1. Words that begin with a vowel, or with the special prefixes "xr" or "yt",
--      simply get "ay" appended.
--   2. For words that start with consonants, move the leading consonant cluster
--      (respecting the special "qu" handling) to the end and append "ay".
--   3. A 'y' functions as a vowel unless it is the first letter of the word.
--   4. Multiple words are preserved and translated individually.
translate :: String -> String
translate = unwords . map translateWord . words
  where
    translateWord :: String -> String
    translateWord w
        | startsWithVowelLike w      = w ++ "ay"
        | otherwise                  =
            let (prefix, rest) = splitConsonantPrefix w
            in rest ++ prefix ++ "ay"

-- Detect if a word starts with a vowel, "xr", or "yt".
startsWithVowelLike :: String -> Bool
startsWithVowelLike word =
    case map toLower word of
        ('x':'r':_) -> True
        ('y':'t':_) -> True
        (c:_)       -> isVowel c
        _           -> False  -- empty word, shouldn't occur with `words`

-- Split a word into (consonantPrefix, rest) following Pig Latin rules.
splitConsonantPrefix :: String -> (String, String)
splitConsonantPrefix word = go "" (map toLower word) word
  where
    go :: String -> String -> String -> (String, String)
    go acc _original [] = (reverse acc, [])  -- shouldn't happen with valid words
    go acc lowered original@(c:cs)
        | null acc && isPrefixOf "qu" lowered
                      = ("qu" ++ reverse acc, drop 2 original)
        | isPrefixOf "qu" lowered
                      = (reverse ('u':'q':acc), drop 2 original)
        | isVowel c   = (reverse acc, original)
        | c == 'y' && not (null acc) -- 'y' acting as vowel
                      = (reverse acc, original)
        | otherwise   = go (c:acc) (tail lowered) cs

-- Helper: check for vowels
isVowel :: Char -> Bool
isVowel ch = toLower ch `elem` ("aeiou" :: String)
