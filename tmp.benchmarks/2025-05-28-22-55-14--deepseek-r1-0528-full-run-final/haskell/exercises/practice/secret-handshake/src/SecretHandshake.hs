module SecretHandshake (handshake) where

import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n = 
  let actions = [(1, "wink"), (2, "double blink"), (4, "close your eyes"), (8, "jump")]
      baseActions = [ action | (bit, action) <- actions, n .&. bit /= 0 ]
  in if n .&. 16 /= 0 then reverse baseActions else baseActions
