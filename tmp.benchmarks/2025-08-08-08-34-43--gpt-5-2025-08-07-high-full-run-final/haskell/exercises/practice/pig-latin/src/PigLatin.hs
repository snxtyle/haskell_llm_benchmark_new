module PigLatin (translate) where

import Data.Char (toLower, isLetter)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord w
  | startsWithVowelLike w = w ++ "ay"
  | otherwise =
      case splitRule3 w of
        Just (prefix, rest) -> rest ++ prefix ++ "ay"
        Nothing ->
          let (prefix, rest) = splitAtBoundary w
          in rest ++ prefix ++ "ay"

-- Rule 1: starts with vowel or "xr"/"yt"
startsWithVowelLike :: String -> Bool
startsWithVowelLike s =
  case s of
    []     -> False
    (c:_)  -> isVowel c || startsWith "xr" s || startsWith "yt" s

isVowel :: Char -> Bool
isVowel c =
  case toLower c of
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    _   -> False

startsWith :: String -> String -> Bool
startsWith pref str =
  let lp = map toLower pref
      ls = map toLower (take (length pref) str)
  in lp == ls

-- Rule 3: zero or more consonants followed by "qu"
-- Returns (movedPrefixIncludingQu, rest) if applicable
splitRule3 :: String -> Maybe (String, String)
splitRule3 s = go [] s
  where
    go acc remStr =
      case remStr of
        a:b:xs
          | toLower a == 'q' && toLower b == 'u'
          -> Just (reverse acc ++ [a,b], xs)
        (c:cs)
          | isConsonant c
          -> go (c:acc) cs
        _ -> Nothing

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

-- Rule 2 and Rule 4 boundary:
-- Move the initial consonant cluster to the end, where "y" acts as a vowel
-- when not at position 0.
splitAtBoundary :: String -> (String, String)
splitAtBoundary s = go [] s 0
  where
    go acc remStr idx =
      case remStr of
        [] -> (reverse acc, [])
        (c:cs)
          | idx > 0 && toLower c == 'y' -> (reverse acc, c:cs)
          | isVowel c                   -> (reverse acc, c:cs)
          | otherwise                   -> go (c:acc) cs (idx + 1)
