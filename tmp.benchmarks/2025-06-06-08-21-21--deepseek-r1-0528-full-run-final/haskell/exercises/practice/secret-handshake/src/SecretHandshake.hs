module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = 
    let actions = [ "wink" | testBit n 0 ] ++
                  [ "double blink" | testBit n 1 ] ++
                  [ "close your eyes" | testBit n 2 ] ++
                  [ "jump" | testBit n 3 ]
    in if testBit n 4 then reverse actions else actions
