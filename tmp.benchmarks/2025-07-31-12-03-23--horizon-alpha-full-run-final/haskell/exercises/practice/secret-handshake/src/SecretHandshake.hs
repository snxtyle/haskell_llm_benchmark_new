module SecretHandshake (handshake) where

import Data.Bits ((.&.), shiftR)

handshake :: Int -> [String]
handshake n =
  let actions =
        [ (0, "wink")
        , (1, "double blink")
        , (2, "close your eyes")
        , (3, "jump")
        ]
      isBitSet i = (n `shiftR` i) .&. 1 == 1
      selected = [ action | (i, action) <- actions, isBitSet i ]
      reverseFlag = isBitSet 4
   in if reverseFlag then reverse selected else selected
