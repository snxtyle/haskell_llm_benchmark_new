module PigLatin (translate) where

import Data.List (isPrefixOf)
import Data.Char (toLower)

-- Function to check if a character is a vowel (a, e, i, o, u)
-- Considers input case-insensitively.
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

-- Translates a single word according to Pig Latin rules
translateWord :: String -> String
translateWord "" = ""
translateWord word@(c:_)
    -- Rule 1: Word begins with a vowel sound (vowel, "xr", "yt")
    -- Note: Haskell's `isPrefixOf` is case-sensitive, assuming input matches "xr", "yt".
    | isVowel c || "xr" `isPrefixOf` word || "yt" `isPrefixOf` word = word ++ "ay"
    -- Rules 2, 3, 4: Word begins with a consonant sound
    | otherwise =
        let (prefix, suffix) = findPrefixSuffix word
        in suffix ++ prefix ++ "ay"

-- Finds the prefix (initial consonant sound cluster including 'qu' or up to 'y')
-- and the rest of the word (suffix).
-- The prefix is the part moved to the end in Pig Latin.
findPrefixSuffix :: String -> (String, String)
findPrefixSuffix word = go word 0
  where
    -- Helper function to recursively find the split point index.
    go :: String -> Int -> (String, String)
    go "" _ = ("", "") -- Base case for empty string during recursion (shouldn't normally hit)
    go w@(c:cs) idx
        -- Vowel found: split occurs *before* the vowel.
        | isVowel c = splitAt idx word
        -- 'y' found after the first letter: treat 'y' as the vowel sound start, split *before* 'y'.
        | c == 'y' && idx > 0 = splitAt idx word
        -- 'qu' found: treat 'qu' as part of the consonant prefix, split *after* 'u'.
        | c == 'q' && not (null cs) && head cs == 'u' = splitAt (idx + 2) word
        -- Consonant found: continue searching in the rest of the string.
        | otherwise = go cs (idx + 1)

-- Translates a full text (potentially multiple words separated by spaces) into Pig Latin
translate :: String -> String
translate text = unwords $ map translateWord $ words text
