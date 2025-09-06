module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n
    | n < 1 || n > 31 = []
    | otherwise = 
        let actions = ["wink", "double blink", "close your eyes", "jump"]
            result = [action | (i, action) <- zip [0..3] actions, testBit n i]
        in if testBit n 4 then reverse result else result
