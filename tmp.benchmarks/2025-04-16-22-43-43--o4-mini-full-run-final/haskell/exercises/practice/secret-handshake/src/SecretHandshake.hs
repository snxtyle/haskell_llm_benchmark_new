module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n =
  let actions = [ "wink"
                , "double blink"
                , "close your eyes"
                , "jump"
                ]
      -- Pick actions for bits 0–3
      selected = [ action
                 | (i, action) <- zip [0..] actions
                 , testBit n i
                 ]
  in if testBit n 4
        then reverse selected
        else selected
