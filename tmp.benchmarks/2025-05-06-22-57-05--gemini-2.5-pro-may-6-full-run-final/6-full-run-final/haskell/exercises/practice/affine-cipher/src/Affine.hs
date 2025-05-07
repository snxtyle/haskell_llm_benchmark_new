module Affine (decode, encode) where

import Data.Char (isDigit, isLower, isUpper, ord, chr)
import Data.List (find, intercalate, unfoldr)
-- gcd is in Prelude

-- m_val is the size of the alphabet
m_val :: Int
m_val = 26

-- Helper: Calculate Modular Multiplicative Inverse
-- mmi a m_val returns x such that (a * x) `mod` m_val == 1.
-- It is guaranteed to exist if gcd(a, m_val) == 1 and m_val > 1.
-- Search space [1..m_val-1] is standard for MMI.
mmi :: Int -> Int -> Maybe Int
mmi a modulus = find (\x -> (a * x) `mod` modulus == 1) [1 .. modulus - 1]

-- Encoding

-- Encodes a single character.
-- Converts alphabetic characters to lowercase, then encrypts.
-- Passes digits through unchanged.
-- Filters out other characters (spaces, punctuation).
encodeChar :: Int -> Int -> Char -> String
encodeChar a b char
  | isLower char = [chr (ord 'a' + (a * (ord char - ord 'a') + b) `mod` m_val)]
  | isUpper char = [chr (ord 'a' + (a * (ord char - ord 'A') + b) `mod` m_val)] -- Output is lowercase
  | isDigit char = [char]
  | otherwise    = [] -- Filter out spaces, punctuation, etc.

-- Groups the encoded string into 5-character chunks separated by spaces.
applyGrouping :: String -> String
applyGrouping str
  | null str = "" -- Handle empty string explicitly to avoid leading/trailing space from intercalate on [""]
  | otherwise = intercalate " " $ unfoldr chunk str
  where
    chunk [] = Nothing -- Stop condition for unfoldr
    chunk s  = Just (take 5 s, drop 5 s)

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | gcd a m_val /= 1 = Nothing -- 'a' must be coprime to m_val.
  | otherwise =
      let processedChars = concatMap (encodeChar a b) plainText
          -- Filter out non-alphanumeric characters before grouping
          onlyAlphanum = filter (\c -> isLower c || isDigit c) processedChars
       in if null onlyAlphanum && not (null (filter (\c -> isLower c || isUpper c || isDigit c) plainText))
          -- This case handles if plaintext had letters/digits but all were filtered by encodeChar (which shouldn't happen with current encodeChar)
          -- or if encodeChar produced something that filter then removed (also unlikely here)
          -- More robustly: if input had encryptable content but output is empty.
          -- The original logic: if encodedChars is empty AND plainText was not empty (meaning it was all non-alphanum)
          then if null (filter (\c -> isLower c || isUpper c || isDigit c) plainText)
               then Just "" -- If plaintext was truly only spaces/punctuation
               else Just $ applyGrouping onlyAlphanum -- Should be ""
          else Just $ applyGrouping onlyAlphanum


-- Decoding

-- Decodes a single character.
-- Assumes alphabetic characters in ciphertext are lowercase.
-- Passes digits through unchanged.
-- Filters out other characters (e.g., spaces from grouping).
decodeChar :: Int -> Int -> Char -> String -- Corrected signature
decodeChar invA b char
  | isLower char = -- Assume ciphertext characters are lowercase letters or digits
      let y = ord char - ord 'a'
          -- Haskell's `mod` behaves as mathematical mod for negative numerators,
          -- e.g., (-5) `mod` 26 == 21.
          decryptedVal = (invA * (y - b)) `mod` m_val
       in [chr (ord 'a' + decryptedVal)]
  | isDigit char = [char]
  | otherwise    = [] -- Filter out spaces (from grouping), punctuation, etc.

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | gcd a m_val /= 1 = Nothing -- 'a' must be coprime to m_val for MMI to exist.
  | otherwise =
      -- MMI exists if and only if a and m_val are coprime (for m_val > 1).
      -- So, if gcd a m_val == 1, mmi will return Just something.
      case mmi a m_val of
        Just invA ->
          let decodedChars = concatMap (decodeChar invA b) cipherText -- Now correct
           in Just decodedChars
        Nothing ->
          -- This branch should theoretically be unreachable if gcd a m_val == 1
          -- and m_val > 1, as MMI is guaranteed to exist.
          -- Returning Nothing is a safe fallback, indicating an issue.
          Nothing
