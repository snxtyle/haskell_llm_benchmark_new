module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n =
  let actions = zip [0..] ["wink", "double blink", "close your eyes", "jump"]
      base = [action | (i, action) <- actions, testBit n i]
  in if testBit n 4 then reverse base else base
