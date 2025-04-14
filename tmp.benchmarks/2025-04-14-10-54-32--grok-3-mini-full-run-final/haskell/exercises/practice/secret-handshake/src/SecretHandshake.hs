module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = 
  let actions = [ if testBit n 0 then "wink" else ""
                , if testBit n 1 then "double blink" else ""
                , if testBit n 2 then "close your eyes" else ""
                , if testBit n 3 then "jump" else ""
                ]
      filteredActions = filter (/= "") actions  -- Remove empty strings for bits that are not set
      reverseFlag = testBit n 4  -- Check if the reverse bit is set
  in if reverseFlag then reverse filteredActions else filteredActions
