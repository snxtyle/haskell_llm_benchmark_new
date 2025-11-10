module PigLatin (translate) where

import Data.List (isPrefixOf, findIndex)
import Data.Char (toLower)

-- | Translates a word or phrase from English to Pig Latin.
translate :: String -> String
translate = unwords . map translateWord . words

-- | Translates a single word to Pig Latin.
translateWord :: String -> String
translateWord [] = []
translateWord word
  -- Rule 1: If a word begins with a vowel, or starts with "xr" or "yt", add an "ay" sound to the end of the word.
  | startsWithVowel word = word ++ "ay"
  | "xr" `isPrefixOf` word = word ++ "ay"
  | "yt" `isPrefixOf` word = word ++ "ay"

  -- Rule 3: If a word starts with zero or more consonants followed by "qu", move those consonants and the "qu" part to the end, and then add an "ay" sound.
  | hasConsonantQu word = moveConsonantQu word

  -- Rule 4: If a word starts with one or more consonants followed by "y", move the consonants preceding the "y" to the end, and then add an "ay" sound.
  | hasConsonantY word = moveConsonantY word

  -- Rule 2: If a word begins with one or more consonants, move those consonants to the end and then add an "ay" sound.
  | otherwise = moveConsonantCluster word

-- Helper function to check if a word starts with a vowel.
startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (c:_) = isVowel c

-- Helper function to check if a character is a vowel.
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

-- Helper function for Rule 3: checks for the consonant + "qu" pattern.
hasConsonantQu :: String -> Bool
hasConsonantQu word =
  let consonants = takeWhile (not . isVowel) word
      rest = drop (length consonants) word
  in "qu" `isPrefixOf` rest

-- Helper function for Rule 3: performs the translation for the consonant + "qu" pattern.
moveConsonantQu :: String -> String
moveConsonantQu word =
  let consonants = takeWhile (not . isVowel) word
      rest = drop (length consonants) word
      prefix = consonants ++ "qu"
      suffix = drop 2 rest
  in suffix ++ prefix ++ "ay"

-- Helper function for Rule 4: checks for the consonant + "y" pattern.
hasConsonantY :: String -> Bool
hasConsonantY word =
  not (startsWithVowel word) && 'y' `elem` word

-- Helper function for Rule 4: performs the translation for the consonant + "y" pattern.
moveConsonantY :: String -> String
moveConsonantY word =
  let Just yIndex = findIndex (== 'y') word
      prefix = take yIndex word
      suffix = drop yIndex word
  in suffix ++ prefix ++ "ay"

-- Helper function for Rule 2: performs the translation for a general consonant cluster.
moveConsonantCluster :: String -> String
moveConsonantCluster word =
  let prefix = takeWhile (not . isVowel) word
      suffix = drop (length prefix) word
  in suffix ++ prefix ++ "ay"
