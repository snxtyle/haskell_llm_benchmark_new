module PigLatin (translate) where

import Data.Char (toLower)

-- | Translate an entire sentence to Pig Latin.
--   Words are assumed to be separated by spaces.
translate :: String -> String
translate = unwords . map translateWord . words

-- | Translate a single word to Pig Latin according to the four rules
--   described in the exercise.
translateWord :: String -> String
translateWord w
  | startsWithVowelSound w = w ++ "ay"
  | otherwise              = rest ++ prefix ++ "ay"
  where
    (prefix, rest) = splitConsonantCluster w

--------------------------------------------------------------------------------
-- Helpers

startsWithVowelSound :: String -> Bool
startsWithVowelSound word =
  case map toLower word of
    []               -> False
    ('x':'r':_)      -> True      -- "xr" rule
    ('y':'t':_)      -> True      -- "yt" rule
    (c:_) | isVowel c -> True
    _                -> False
  where
    isVowel c = c `elem` "aeiou"

-- | Split a word into its leading consonant cluster (according to the Pig Latin
--   rules) and the rest of the word.
--
--   The function returns a tuple (consonantCluster, remainder).
splitConsonantCluster :: String -> (String, String)
splitConsonantCluster = go ""
  where
    go acc rest@(x:xs)
      -- Rule 3: treat "qu" (possibly preceded by consonants) as part of the cluster
      | map toLower (take 2 rest) == "qu" = (acc ++ take 2 rest, drop 2 rest)

      -- Stop when we encounter a vowel
      | isVowel x = (acc, rest)

      -- Rule 4: 'y' acts like a vowel unless it is the first character
      | toLower x == 'y' && not (null acc) = (acc, rest)

      -- Still in the consonant cluster
      | otherwise = go (acc ++ [x]) xs

    go acc [] = (acc, [])

    isVowel c = toLower c `elem` "aeiou"
