module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord [] = "ay"
translateWord word@(c:_)
  | c `elem` "aeiou" = word ++ "ay"
  | take 2 word `elem` ["xr", "yt"] = word ++ "ay"
translateWord word = moveConsonantSound word ++ "ay"

moveConsonantSound :: String -> String
moveConsonantSound word
  | hasYAfterConsonant word = 
      let (consonants, rest) = span (`notElem` "aeiou") (tail word)
          firstConsonant = [head word]
      in if head rest == 'y'
           then (tail rest ++ firstConsonant ++ consonants)
           else moveRegularConsonants word
  | otherwise = moveRegularConsonants word
  where
    hasYAfterConsonant w = 
      not (null w) && head w `notElem` "aeiou" && 'y' `elem` w && 
      let (consonants, rest) = span (`notElem` "aeiou") w
      in not (null consonants) && not (null rest) && head rest == 'y'

moveRegularConsonants :: String -> String
moveRegularConsonants word = 
  let (consonants, rest) = span (`notElem` "aeiou") word
  in case rest of
       ('u':xs) | not (null consonants) && last consonants == 'q' -> 
         xs ++ init consonants ++ "qu"
       _ -> rest ++ consonants
