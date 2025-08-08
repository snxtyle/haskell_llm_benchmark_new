module PigLatin (translate) where

import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord w
  | null w = w
  -- Rule 1: starts with a vowel, or "xr"/"yt" -> append "ay"
  | isVowelStart w = w ++ "ay"
  -- Rule 3: zero or more consonants followed by "qu"
  | Just n <- findQuSplit w =
      let (prefix, rest) = splitAt n w
      in rest ++ prefix ++ "ay"
  -- Rule 4: one or more consonants followed by 'y'
  | Just (cs, yRest) <- splitAtYAfterConsonants w = yRest ++ cs ++ "ay"
  -- Rule 2: leading consonant cluster
  | otherwise =
      let (cons, rest) = span (not . isVowel) w
      in rest ++ cons ++ "ay"

-- Helpers

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

isVowelStart :: String -> Bool
isVowelStart s =
  case s of
    (x:_) | isVowel x -> True
    _ | "xr" `isPrefixOf` s -> True
      | "yt" `isPrefixOf` s -> True
    _ -> False

-- Find the split position right after the initial (consonants + "qu") pattern, if present.
-- Returns Just index to split at, or Nothing if not matching rule 3.
findQuSplit :: String -> Maybe Int
findQuSplit = go 0
  where
    go :: Int -> String -> Maybe Int
    go i ('q':'u':_) = Just (i + 2)
    go _ (c:_) | isVowel c = Nothing
    go i (_:xs) = go (i + 1) xs
    go _ [] = Nothing

-- If word starts with one or more consonants (excluding 'y' from this group) followed by 'y',
-- return (consonants, restStartingWithY). Otherwise Nothing.
splitAtYAfterConsonants :: String -> Maybe (String, String)
splitAtYAfterConsonants s =
  let cs = takeWhile (\c -> not (isVowel c) && c /= 'y') s
      rest = drop (length cs) s
  in case rest of
       ('y':_) | not (null cs) -> Just (cs, rest)
       _ -> Nothing
