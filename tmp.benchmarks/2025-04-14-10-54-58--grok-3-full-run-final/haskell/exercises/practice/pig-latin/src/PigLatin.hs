module PigLatin (translate) where

translate :: String -> String
translate input = unwords $ map translateWord (words input)

translateWord :: String -> String
translateWord word
  | isVowelStart word || isPrefix "xr" word || isPrefix "yt" word = word ++ "ay"
  | hasQuPrefix (snd $ splitOnFirstVowelOrY word) = handleQuWord word
  | hasYPrefixAfterConsonants word = handleYWord word
  | otherwise = handleConsonantWord word

isVowel :: Char -> Bool
isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

isVowelStart :: String -> Bool
isVowelStart word = not (null word) && isVowel (head word)

isPrefix :: String -> String -> Bool
isPrefix prefix word = take (length prefix) word == prefix

splitOnFirstVowelOrY :: String -> (String, String)
splitOnFirstVowelOrY word = span (not . isVowelOrY) word
  where
    isVowelOrY c = isVowel c || c == 'y' || c == 'Y'

hasQuPrefix :: String -> Bool
hasQuPrefix rest = isPrefix "qu" rest || isPrefix "QU" rest

hasYPrefixAfterConsonants :: String -> Bool
hasYPrefixAfterConsonants word = 
  let (consonants, rest) = splitOnFirstVowelOrY word
  in not (null consonants) && not (null rest) && (head rest == 'y' || head rest == 'Y')

handleQuWord :: String -> String
handleQuWord word = 
  let (start, rest) = splitAt (length (fst (splitOnFirstVowelOrY word)) + 2) word
      newWord = rest ++ start
  in newWord ++ "ay"

handleYWord :: String -> String
handleYWord word = 
  let (consonants, rest) = splitOnFirstVowelOrY word
      newWord = rest ++ consonants
  in newWord ++ "ay"

handleConsonantWord :: String -> String
handleConsonantWord word = 
  let (consonants, rest) = splitOnFirstVowelOrY word
      newWord = rest ++ consonants
  in newWord ++ "ay"
