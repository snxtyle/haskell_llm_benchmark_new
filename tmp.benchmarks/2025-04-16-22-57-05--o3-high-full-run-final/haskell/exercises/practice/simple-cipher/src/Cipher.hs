module Cipher
  ( -- high‑level API expected by тесты
    encode
  , decode
  , randomKey
    -- дополнительные экспортируемые функции
  , caesarEncode
  , caesarDecode
  , caesarEncodeRandom
  ) where

import           Control.Monad   (replicateM)
import           Data.Char       (chr, isLower, ord)
import           System.Random   (randomRIO)

-- | Convert a lowercase letter to 0‑25.
charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

-- | Convert 0‑25 back to a lowercase letter.
intToChar :: Int -> Char
intToChar n = chr (ord 'a' + n `mod` 26)

-- | Shift a lowercase char forward by @n@ positions.
shiftForward :: Int -> Char -> Char
shiftForward n c
  | isLower c = intToChar $ charToInt c + n
  | otherwise = c

-- | Shift a lowercase char backward by @n@ positions.
shiftBackward :: Int -> Char -> Char
shiftBackward n c
  | isLower c = intToChar $ charToInt c - n
  | otherwise = c

-- | Encode plaintext with the given key.
--   The key is cycled to match the length of the text.
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith encodeChar text (cycle key)
  where
    encodeChar p k = shiftForward (charToInt k) p

-- | Decode ciphertext with the given key.
caesarDecode :: String -> String -> String
caesarDecode key cipher = zipWith decodeChar cipher (cycle key)
  where
    decodeChar c k = shiftBackward (charToInt k) c

-- | Проверка, что ключ непустой и состоит только из строчных букв.
validKey :: String -> Bool
validKey k = not (null k) && all isLower k

-- | API, используемый в тестах Exercism.
encode :: String -> String -> String
encode key plaintext
  | validKey key = caesarEncode key plaintext
  | otherwise    = error "encode: key must be non‑empty lowercase letters"

decode :: String -> String -> String
decode key ciphertext
  | validKey key = caesarDecode key ciphertext
  | otherwise    = error "decode: key must be non‑empty lowercase letters"

-- | Сгенерировать случайный ключ длиной ≥100 символов.
randomKey :: IO String
randomKey = generateKey 100

-- | Generate a random key (≥100 chars) and encode the given text with it.
--   Returns @(key, ciphertext)@.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey
  pure (key, caesarEncode key text)

-- | Helper: generate a random lowercase key of the requested length.
generateKey :: Int -> IO String
generateKey n = replicateM n (randomRIO ('a', 'z'))
