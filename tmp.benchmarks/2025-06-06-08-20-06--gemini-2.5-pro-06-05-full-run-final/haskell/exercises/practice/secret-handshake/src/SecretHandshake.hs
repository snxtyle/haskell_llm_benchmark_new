module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n =
  let
    actions = concat [
        if testBit n 0 then ["wink"] else [],
        if testBit n 1 then ["double blink"] else [],
        if testBit n 2 then ["close your eyes"] else [],
        if testBit n 3 then ["jump"] else []
      ]
  in
    if testBit n 4
    then reverse actions
    else actions
