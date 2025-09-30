module PigLatin (translate) where

import Data.List (isPrefixOf)

translate :: String -> String
translate xs = unwords (map translateWord (words xs))

translateWord :: String -> String
translateWord word
  | startsWithVowel word = word ++ "ay"
  | "xr" `isPrefixOf` word = word ++ "ay"
  | "yt" `isPrefixOf` word = word ++ "ay"
  | otherwise = moveConsonantsAndAddAy word

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (c:_) = c `elem` "aeiouAEIOU"

moveConsonantsAndAddAy :: String -> String
moveConsonantsAndAddAy word = 
  let (consonants, rest) = splitAtConsonantCluster word
  in rest ++ consonants ++ "ay"

splitAtConsonantCluster :: String -> (String, String)
splitAtConsonantCluster word = go "" word
  where
    go acc [] = (acc, [])
    go acc remaining@(c:cs)
      | c `elem` "aeiouAEIOU" = (acc, remaining)
      | "qu" `isPrefixOf` remaining = (acc ++ "qu", drop 2 remaining)
      | c == 'y' && not (null acc) = (acc, remaining)
      | otherwise = go (acc ++ [c]) cs
