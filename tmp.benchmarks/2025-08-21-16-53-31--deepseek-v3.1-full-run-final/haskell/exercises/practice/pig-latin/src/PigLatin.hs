module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord [] = []
translateWord word
  | startsWithVowelSound word = word ++ "ay"
  | hasQUPattern word = handleQUPattern word
  | hasYAfterConsonants word = handleYPattern word
  | otherwise = handleConsonants word

startsWithVowelSound :: String -> Bool
startsWithVowelSound [] = False
startsWithVowelSound (x:xs)
  | x `elem` "aeiou" = True
  | x == 'x' && not (null xs) && xs !! 0 == 'r' = True
  | x == 'y' && not (null xs) && xs !! 0 == 't' = True
  | otherwise = False

hasQUPattern :: String -> Bool
hasQUPattern word = case word of
  'q':'u':_ -> True
  c:cs | c `elem` consonants -> hasQUPattern cs
  _ -> False

hasYAfterConsonants :: String -> Bool
hasYAfterConsonants word = case word of
  'y':_ -> False  -- y at start is a consonant, not following consonants
  _ -> 
    let (consonantPart, rest) = span (`elem` consonants) word
    in not (null consonantPart) && not (null rest) && head rest == 'y'

handleQUPattern :: String -> String
handleQUPattern word = 
  let (beforeQU, fromQ) = break (== 'q') word
  in if not (null fromQ) && length fromQ > 1 && fromQ !! 1 == 'u'
     then drop 2 fromQ ++ beforeQU ++ "quay"
     else word ++ "ay"  -- fallback

handleYPattern :: String -> String
handleYPattern word =
  let (consonantPart, rest) = span (`elem` consonants) word
  in if not (null rest) && head rest == 'y'
     then rest ++ consonantPart ++ "ay"
     else word ++ "ay"  -- fallback

handleConsonants :: String -> String
handleConsonants word =
  let (consonantPart, rest) = span (`elem` consonants) word
  in rest ++ consonantPart ++ "ay"

consonants :: String
consonants = "bcdfghjklmnpqrstvwxz"
