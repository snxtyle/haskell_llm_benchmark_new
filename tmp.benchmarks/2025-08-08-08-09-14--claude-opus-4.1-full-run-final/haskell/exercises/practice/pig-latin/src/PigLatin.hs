module PigLatin (translate) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
    -- Rule 1: starts with vowel, "xr", or "yt"
    | startsWithVowel word = word ++ "ay"
    | "xr" `isPrefixOf` word = word ++ "ay"
    | "yt" `isPrefixOf` word = word ++ "ay"
    -- Rule 3: consonants followed by "qu" (must check before Rule 2)
    | handleQu word /= Nothing = case handleQu word of
        Just result -> result
        Nothing -> error "Unexpected Nothing"
    -- Rule 4: consonants followed by "y" (must check before Rule 2)
    | handleY word /= Nothing = case handleY word of
        Just result -> result
        Nothing -> error "Unexpected Nothing"
    -- Rule 2: starts with consonants
    | otherwise = moveConsonantsToEnd word ++ "ay"

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (c:_) = toLower c `elem` "aeiou"

isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

isConsonant :: Char -> Bool
isConsonant c = not (isVowel c) && c `elem` ['a'..'z'] ++ ['A'..'Z']

-- Handle Rule 3: consonants + "qu"
handleQu :: String -> Maybe String
handleQu word
    -- Check if word starts with "qu"
    | "qu" `isPrefixOf` word = Just $ drop 2 word ++ "qu" ++ "ay"
    -- Check for consonants followed by "qu"
    | otherwise = 
        let consonants = takeWhile isConsonant word
            afterConsonants = drop (length consonants) word
        in if not (null consonants) && "qu" `isPrefixOf` afterConsonants
           then Just $ drop 2 afterConsonants ++ consonants ++ "qu" ++ "ay"
           else Nothing

-- Handle Rule 4: consonants + "y"
handleY :: String -> Maybe String
handleY word =
    let consonants = takeWhile isConsonant word
        afterConsonants = drop (length consonants) word
    in case afterConsonants of
        ('y':_) | not (null consonants) -> Just $ afterConsonants ++ consonants ++ "ay"
        _ -> Nothing

-- Handle Rule 2: move leading consonants to end
moveConsonantsToEnd :: String -> String
moveConsonantsToEnd word =
    let consonants = takeWhile isConsonant word
        rest = drop (length consonants) word
    in rest ++ consonants
