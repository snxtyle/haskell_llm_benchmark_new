module PigLatin (translate) where

import Data.Char (toLower)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isConsonant :: Char -> Bool
isConsonant c = not (isVowel c)  -- Assuming the character is a letter; we don't check isAlpha for simplicity

translate :: String -> String
translate xs = translate' (map toLower xs)
  where
    translate' :: String -> String
    translate' [] = []  -- Empty string returns empty
    translate' str =
      if not (null str) && (isVowel (head str) || take 2 str == "xr" || take 2 str == "yt")  -- Rule 1
        then str ++ "ay"
      else
        let prefix = takeWhile isConsonant str  -- Take initial consonants
        in if take 2 str == "qu"  -- Check if word starts with "qu"
             then let toMove = "qu"
                      rest = drop 2 str
                  in rest ++ toMove ++ "ay"
           else if length prefix > 0 && length str >= length prefix + 2 && take 2 (drop (length prefix) str) == "qu"  -- Rule 3 for after consonants
             then let toMove = prefix ++ "qu"
                      rest = drop (length prefix + 2) str
                  in rest ++ toMove ++ "ay"
           else if length prefix > 0 && not (null (drop (length prefix) str)) && head (drop (length prefix) str) == 'y'  -- Rule 4
                  then let afterPrefix = drop (length prefix) str
                       in afterPrefix ++ prefix ++ "ay"
                else  -- Rule 2 (catch-all for starts with consonants)
                  let rest = drop (length prefix) str
                  in rest ++ prefix ++ "ay"
