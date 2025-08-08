module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = 
    let actions = getActions n
    in if testBit n 4  -- Check bit 5 (0-indexed, so position 4)
       then reverse actions
       else actions

getActions :: Int -> [String]
getActions n = 
    concat [ if testBit n 0 then ["wink"] else []
           , if testBit n 1 then ["double blink"] else []
           , if testBit n 2 then ["close your eyes"] else []
           , if testBit n 3 then ["jump"] else []
           ]
