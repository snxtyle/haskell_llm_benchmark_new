module PigLatin (translate) where

import Data.Char (isLetter)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
    | startsWithVowel word = word ++ "ay"
    | startsWithXrOrYt word = word ++ "ay"
    | otherwise = applyConsonantRules word

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (c:_) = c `elem` "aeiouAEIOU"

startsWithXrOrYt :: String -> Bool
startsWithXrOrYt word = take 2 word `elem` ["xr", "yt"]

applyConsonantRules :: String -> String
applyConsonantRules word
    | hasQuAfterConsonants word = handleQu word
    | hasYAfterConsonants word = handleY word
    | otherwise = moveConsonantsToEnd word

hasQuAfterConsonants :: String -> Bool
hasQuAfterConsonants word =
    let consonants = takeWhile isConsonant word
        remaining = drop (length consonants) word
    in take 2 remaining == "qu" || (null consonants && take 2 word == "qu")

hasYAfterConsonants :: String -> Bool
hasYAfterConsonants [] = False
hasYAfterConsonants (c:rest)
    | isConsonant c && not (null rest) && head rest == 'y' = True
    | isConsonant c = hasYAfterConsonants rest
    | otherwise = False

handleQu :: String -> String
handleQu word =
    let consonants = takeWhile isConsonant word
        afterConsonants = drop (length consonants) word
    in if null consonants && take 2 word == "qu"
       then drop 2 word ++ "qu" ++ "ay"
       else let (qu, rest) = splitAt 2 afterConsonants
            in rest ++ consonants ++ qu ++ "ay"

handleY :: String -> String
handleY word =
    let consonantsBeforeY = takeWhile (\c -> isConsonant c && c /= 'y') word
        afterConsonants = drop (length consonantsBeforeY) word
    in afterConsonants ++ consonantsBeforeY ++ "ay"

moveConsonantsToEnd :: String -> String
moveConsonantsToEnd word =
    let consonants = takeWhile isConsonant word
        rest = dropWhile isConsonant word
    in rest ++ consonants ++ "ay"

isConsonant :: Char -> Bool
isConsonant c = isLetter c && c `notElem` "aeiouAEIOU"
