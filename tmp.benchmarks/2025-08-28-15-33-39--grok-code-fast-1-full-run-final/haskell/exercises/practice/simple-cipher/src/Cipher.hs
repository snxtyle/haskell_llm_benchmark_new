module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (chr, ord, isLower)
import System.Random (randomRIO)
import Control.Monad (replicateM)

keyShift :: Char -> Int
keyShift c = ord c - ord 'a'

shiftChar :: Int -> Char -> Char
shiftChar n c
  | isLower c = chr (ord 'a' + (ord c - ord 'a' + n) `mod` 26)
  | otherwise = c

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith (\t k -> shiftChar (keyShift k) t) text (cycle key)

caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith (\e k -> shiftChar (-keyShift k) e) encodedText (cycle key)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- replicateM 100 (randomRIO ('a', 'z'))
  let encoded = caesarEncode key text
  return (key, encoded)
