module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower, toLower)
import System.Random (randomRIO)
import Control.Monad (replicateM)

caesarDecode :: String -> String -> String
caesarDecode key encodedText = 
  let shifts = map (charToShift . toLower) (cycle key)
  in zipWith decodeChar encodedText shifts
  where
    decodeChar c shift = shiftChar (-shift) c

caesarEncode :: String -> String -> String
caesarEncode key text = 
  let shifts = map (charToShift . toLower) (cycle key)
  in zipWith shiftChar shifts text

charToShift :: Char -> Int
charToShift c = ord c - ord 'a'

shiftChar :: Int -> Char -> Char
shiftChar s c 
  | isLower c = 
      let base = ord 'a'
          pos = (ord c - base + s) `mod` 26
      in chr $ base + pos
  | otherwise = c

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateKey 100
  let encoded = caesarEncode key text
  return (key, encoded)

generateKey :: Int -> IO String
generateKey n = replicateM n (randomRIO ('a','z'))
