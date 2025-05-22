module PigLatin (translate) where

import Data.List (isPrefixOf)

translate :: String -> String
translate input = unwords (map translateWord (words input))

translateWord :: String -> String
translateWord xs
  | startsWithVowel xs = xs ++ "ay"
  | isPrefixOf "xr" xs = xs ++ "ay"
  | isPrefixOf "yt" xs = xs ++ "ay"
  | otherwise = translateConsonantStart xs

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:_) = x `elem` "aeiou"

translateConsonantStart :: String -> String
translateConsonantStart xs = 
  let (consonants, rest) = splitConsonants xs
  in case rest of
    ('q':'u':remainder) -> remainder ++ consonants ++ "qu" ++ "ay"
    ('y':remainder) -> 'y':remainder ++ consonants ++ "ay"
    _ -> rest ++ consonants ++ "ay"

splitConsonants :: String -> (String, String)
splitConsonants xs = splitConsonants' xs ""
  where
    splitConsonants' [] acc = (acc, [])
    splitConsonants' (y:ys) acc
      | y `elem` "aeiou" = (acc, y:ys)
      | y == 'y' && not (null acc) = (acc, y:ys)
      | y == 'q' && not (null ys) && head ys == 'u' = (acc, y:ys)
      | otherwise = splitConsonants' ys (acc ++ [y])
