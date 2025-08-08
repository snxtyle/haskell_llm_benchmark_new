module SecretHandshake (handshake) where

import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n =
  let m = n .&. 31
      steps =
        [ (1, "wink")
        , (2, "double blink")
        , (4, "close your eyes")
        , (8, "jump")
        ]
      base = [ s | (mask, s) <- steps, m .&. mask /= 0 ]
  in if m .&. 16 /= 0 then reverse base else base
