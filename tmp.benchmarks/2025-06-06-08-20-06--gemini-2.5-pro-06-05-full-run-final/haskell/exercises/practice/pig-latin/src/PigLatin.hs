module PigLatin (translate) where

import Data.List (isPrefixOf)

-- | Translates a sentence from English to Pig Latin.
translate :: String -> String
translate = unwords . map translateWord . words

-- | Translates a single word to Pig Latin.
translateWord :: String -> String
translateWord w =
  let idx = findSplitIndex w
      (prefix, suffix) = splitAt idx w
  in suffix ++ prefix ++ "ay"

-- | Finds the split index in a word between the initial consonant cluster
-- and the rest of the word.
findSplitIndex :: String -> Int
findSplitIndex "" = 0
findSplitIndex w@(c:_)
  -- Rule 1: Word begins with a vowel sound.
  | isVowel c || "xr" `isPrefixOf` w || "yt" `isPrefixOf` w = 0
  -- Rules 2, 3, 4: Word begins with a consonant sound.
  | otherwise = go 0 w
  where
    go :: Int -> String -> Int
    go i "" = i
    go i s@(x:xs)
      -- 'y' is a vowel if not at the start of the word.
      | i > 0 && x == 'y' = i
      | isVowel x = i
      -- 'qu' is a consonant cluster.
      | x == 'q' && not (null xs) && head xs == 'u' = go (i+2) (tail xs)
      | otherwise = go (i+1) xs

-- | Checks if a character is a vowel.
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"
