module PigLatin (translate) where

import Data.Char (isLetter, toLower)

-- Helper to check if a character is a vowel
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

-- Helper to check if a character is a consonant
-- A character is a consonant if it's a letter and not a vowel.
isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

-- Helper to take leading consonants
takeLeadingConsonants :: String -> String
takeLeadingConsonants = takeWhile isConsonant

-- Helper to drop leading consonants
dropLeadingConsonants :: String -> String
dropLeadingConsonants = dropWhile isConsonant

-- Helper to find a "qu" cluster (e.g., "qu" or "squ")
-- Returns Just (clusterToMove, restOfWord) if found, Nothing otherwise.
-- The clusterToMove includes the leading consonants (if any) and "qu".
findQuCluster :: String -> Maybe (String, String)
findQuCluster s = go [] s
  where
    go acc (c1:c2:rest)
        | c1 == 'q' && c2 == 'u' = Just (reverse acc ++ "qu", rest)
        | isConsonant c1         = go (c1:acc) (c2:rest)
        | otherwise              = Nothing -- Not a consonant, or not 'qu'
    go _ _ = Nothing -- Not enough characters for "qu"

-- Helper to find a consonant cluster followed by 'y' (e.g., "my", "rhythm")
-- Returns Just (consonantPrefix, restOfWordIncludingY) if found, Nothing otherwise.
-- The 'y' itself is NOT part of the moved prefix.
findConsonantYCluster :: String -> Maybe (String, String)
findConsonantYCluster s = go [] s
  where
    go acc (c:cs)
        | c == 'y' && not (null acc) = Just (reverse acc, c:cs) -- Found 'y' preceded by consonants
        | isConsonant c              = go (c:acc) cs             -- Accumulate consonants
        | otherwise                  = Nothing                   -- Not a consonant, or 'y' not preceded by consonants
    go _ [] = Nothing -- End of string, no 'y' found or no preceding consonants

-- The main translation function for a single word
translate :: String -> String
translate [] = []
translate word@(x:xs)
    -- Rule 1: If a word begins with a vowel, or starts with "xr" or "yt"
    | isVowel x || take 2 word == "xr" || take 2 word == "yt" = word ++ "ay"

    -- Rule 3: If a word starts with zero or more consonants followed by "qu"
    | Just (cluster, rest) <- findQuCluster word = rest ++ cluster ++ "ay"

    -- Rule 4: If a word starts with one or more consonants followed by "y"
    | Just (consonantPrefix, restAfterY) <- findConsonantYCluster word =
        restAfterY ++ consonantPrefix ++ "ay"

    -- Rule 2: If a word begins with one or more consonants (general case)
    | not (null (takeLeadingConsonants word)) =
        let consonants = takeLeadingConsonants word
            rest = dropLeadingConsonants word
        in rest ++ consonants ++ "ay"

    -- Fallback for words that don't fit rules (e.g., single non-letter char, or empty string already handled)
    -- This case should ideally not be reached for valid English words.
    | otherwise = word
