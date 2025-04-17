module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, toLower, ord, chr)

-- | Decode an Atbash-encoded string: remove non-alphanumeric chars,
-- apply Atbash substitution, and return the plain text.
decode :: String -> String
decode cipherText = map atbashChar filtered
  where
    -- Keep only letters and digits
    filtered = filter isAlphaNum cipherText

-- | Encode a plain text string with the Atbash cipher:
-- 1. Lowercase everything
-- 2. Remove non-alphanumeric chars
-- 3. Apply Atbash substitution
-- 4. Group into blocks of 5 characters separated by spaces
encode :: String -> String
encode plainText = unwords (group5 substituted)
  where
    -- Lowercase, then keep only letters and digits
    filtered = filter isAlphaNum $ map toLower plainText
    -- Apply Atbash to each character
    substituted = map atbashChar filtered

-- | Atbash substitution: letters are mapped a↔z, b↔y, etc.; digits unchanged.
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c =
      let lc = toLower c
      in chr (ord 'a' + ord 'z' - ord lc)
  | otherwise = c

-- | Split a list into chunks of 5 characters
group5 :: [a] -> [[a]]
group5 [] = []
group5 xs = let (h, t) = splitAt 5 xs
            in h : group5 t
