module PigLatin (translate) where

import Data.Char (toLower)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord w
  | startsWithVowelLike lw = w ++ "ay"
  | otherwise =
      case splitAtCluster lw of
        (pre, post) -> post ++ pre ++ "ay"
  where
    lw = map toLower w

-- Rule 1: starts with vowel, or with "xr" or "yt"
startsWithVowelLike :: String -> Bool
startsWithVowelLike s =
  case s of
    []      -> True
    (c:_)   -> isVowelStart s c || startsWithXR s || startsWithYT s
  where
    isVowelStart _ c = c `elem` "aeiou"
    startsWithXR x = take 2 x == "xr"
    startsWithYT x = take 2 x == "yt"

-- Split the word into (leading cluster to move, rest)
-- Implements Rules 2, 3, and 4 including "qu" handling and 'y' as vowel after consonants.
splitAtCluster :: String -> (String, String)
splitAtCluster s = go "" s
  where
    go acc [] = (acc, [])
    -- Handle Rule 3: any consonants followed by "qu" move together
    go acc rem@(c1:c2:cs)
      | isConsonant c1 && c2 == 'u' && c1 == 'q' = (acc ++ "qu", cs) -- starts with "qu"
      | not (null acc) && c1 == 'q' && c2 == 'u' = (acc ++ "qu", cs)  -- consonants then "qu"
      | otherwise =
          step acc rem
    go acc rem = step acc rem

    step acc (c:cs)
      -- Rule 4: 'y' after one or more consonants acts as vowel boundary
      | c == 'y' && not (null acc) = (acc, c:cs)
      -- Continue consuming consonants
      | isConsonant c =
          -- Special-case: if after consuming some consonants we see "qu", handle in next iteration
          case cs of
            ('q':'u':rest) -> (acc ++ [c] ++ "qu", rest)
            _              -> go (acc ++ [c]) cs
      -- Hit a vowel -> stop
      | otherwise = (acc, c:cs)
    step acc [] = (acc, [])

isConsonant :: Char -> Bool
isConsonant c = c `elem` consonants

consonants :: [Char]
consonants = ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','y','z']
