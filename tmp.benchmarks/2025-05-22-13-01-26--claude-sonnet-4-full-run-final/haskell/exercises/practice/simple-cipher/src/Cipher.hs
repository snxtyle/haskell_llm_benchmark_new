module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (ord, chr)

caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith decodeChar (cycle key) encodedText
  where
    decodeChar keyChar encodedChar = 
      let keyShift = ord keyChar - ord 'a'
          encodedPos = ord encodedChar - ord 'a'
          originalPos = (encodedPos - keyShift) `mod` 26
      in chr (originalPos + ord 'a')

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith encodeChar (cycle key) text
  where
    encodeChar keyChar plainChar = 
      let keyShift = ord keyChar - ord 'a'
          plainPos = ord plainChar - ord 'a'
          encodedPos = (plainPos + keyShift) `mod` 26
      in chr (encodedPos + ord 'a')

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  let encoded = caesarEncode key text
  return (key, encoded)

generateRandomKey :: Int -> IO String
generateRandomKey len = sequence $ replicate len randomLowerChar
  where
    randomLowerChar = do
      n <- randomRIO (0, 25)
      return $ chr (n + ord 'a')
