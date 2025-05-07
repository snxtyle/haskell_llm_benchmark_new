module PigLatin (translate) where

import Data.Char (toLower)

-- | Translate a (possibly multi‑word) string from English to Pig Latin.
translate :: String -> String
translate = unwords . map translateWord . words

-- | Translate a single word according to the rules described in the README.
translateWord :: String -> String
translateWord w
  | beginsWith "xr" w = w ++ "ay"              -- rule 1 (special vowel‑sounds)
  | beginsWith "yt" w = w ++ "ay"              -- rule 1 (special vowel‑sounds)
  | startsWithVowel w = w ++ "ay"              -- rule 1 (regular vowels)
  | otherwise          = rest ++ prefix ++ "ay"
  where
    (prefix, rest) = splitPrefix w

-- Determine whether the word starts with a vowel.
startsWithVowel :: String -> Bool
startsWithVowel (c:_) = isVowel c
startsWithVowel _     = False

-- Helper that checks whether a word begins with a specific prefix.
beginsWith :: String -> String -> Bool
beginsWith pfx str = map toLower pfx == take (length pfx) (map toLower str)

-- Split the word into its initial consonant cluster (including the “qu”
-- handling and “y”-as-a‑vowel rule) and the remainder of the word.
--
-- The function returns a tuple (prefix, rest) where:
--   prefix – the part to move to the end
--   rest   – the part that stays at the front
splitPrefix :: String -> (String, String)
splitPrefix word
  | null word = ("", "")
  | otherwise = let len = prefixLength 0 word
                in splitAt len word

-- Compute the length of the prefix to be moved.
prefixLength :: Int -> String -> Int
prefixLength _ [] = 0
prefixLength idx s@(c1:c2:cs)
  -- Handle the “qu” rule: once we see “qu”, include it in the prefix and stop.
  | c1 == 'q' && toLower c2 == 'u' = idx + 2
  -- “y” acts as a vowel when it is *not* the first character.
  | idx > 0 && c1 == 'y'           = idx
  -- Stop at the first vowel.
  | isVowel c1                     = idx
  -- Otherwise, keep scanning.
  | otherwise                      = prefixLength (idx + 1) (c2:cs)
prefixLength idx [c1]
  | idx > 0 && c1 == 'y' = idx
  | isVowel c1           = idx
  | otherwise            = idx + 1

-- Simple vowel predicate (a, e, i, o, u).
isVowel :: Char -> Bool
isVowel ch = toLower ch `elem` "aeiou"
