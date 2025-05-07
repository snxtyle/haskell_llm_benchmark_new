module PigLatin (translate) where

import Data.List (isPrefixOf)

-- Definition of a vowel (a, e, i, o, u)
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

-- Rule 1: Check if a word starts with a vowel sound ("a", "e", "i", "o", "u", "xr", "yt")
startsWithVowelSound :: String -> Bool
startsWithVowelSound "" = False -- Empty string doesn't start with a vowel sound
startsWithVowelSound word@(c:_) =
    isVowel c || isPrefixOf "xr" word || isPrefixOf "yt" word

-- Translates a single word to Pig Latin
translateWord :: String -> String
translateWord "" = "" -- Empty word translates to empty word
translateWord word
    -- Rule 1: If a word begins with a vowel sound, add "ay"
    | startsWithVowelSound word = word ++ "ay"
    | otherwise =
        -- Word does not start with a vowel sound (it starts with a consonant or consonant cluster).
        -- The order of checking subsequent rules matters.

        -- Check for Rule 3: Starts with (zero or more consonants) followed by "qu".
        -- The consonants here are any non-vowel characters except 'q' itself if it's part of "qu".
        let (consonantsBeforeQuCandidate, afterConsonantsBeforeQu) = span (\x -> not (isVowel x) && x /= 'q') word
        in if length afterConsonantsBeforeQu >= 2 && take 2 afterConsonantsBeforeQu == "qu" then
            -- Rule 3 applies: move consonantsBeforeQuCandidate and "qu" to the end.
            let stem = drop 2 afterConsonantsBeforeQu
            in stem ++ consonantsBeforeQuCandidate ++ "qu" ++ "ay"
        else
            -- Rule 3 does not apply. Check for Rule 4.
            -- Rule 4: Starts with one or more consonants followed by "y" (where "y" acts as a vowel).
            -- The consonants here are any non-vowel characters except 'y' itself.
            let (leadingConsonantsBeforeY, afterLeadingConsonantsBeforeY) = span (\x -> not (isVowel x) && x /= 'y') word
            in if not (null leadingConsonantsBeforeY) && (not (null afterLeadingConsonantsBeforeY) && head afterLeadingConsonantsBeforeY == 'y') then
                -- Rule 4 applies: 'y' is the first vowel sound in this context. Move leadingConsonantsBeforeY.
                afterLeadingConsonantsBeforeY ++ leadingConsonantsBeforeY ++ "ay"
            else
                -- Rule 1, 3, and 4 do not apply. Must be Rule 2.
                -- Rule 2: Starts with consonant(s), move all initial consonants (up to the first true vowel a,e,i,o,u) to the end.
                let (initialConsonantsAll, restOfWordAfterVowel) = span (not . isVowel) word
                -- Since Rule 1 failed, initialConsonantsAll must be non-empty.
                in restOfWordAfterVowel ++ initialConsonantsAll ++ "ay"

-- Main translation function: splits sentence into words, translates each, joins them back
translate :: String -> String
translate xs = unwords (map translateWord (words xs))
