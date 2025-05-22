module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = 
    let actions = [ "wink"
                  , "double blink"
                  , "close your eyes"
                  , "jump"
                  ]
        -- Check bits 0-3 and collect corresponding actions
        baseActions = [action | (i, action) <- zip [0..3] actions, testBit n i]
        -- Check bit 4 for reverse
        shouldReverse = testBit n 4
    in if shouldReverse
       then reverse baseActions
       else baseActions
