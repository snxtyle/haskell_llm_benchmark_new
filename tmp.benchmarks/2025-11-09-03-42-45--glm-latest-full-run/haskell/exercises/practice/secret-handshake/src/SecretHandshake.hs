module SecretHandshake (handshake) where

import Data.Bits (Bits((.&.)))

-- | Convert a number to a secret handshake sequence.
-- The sequence is determined by the rightmost five digits of the number's binary representation.
-- 1 = wink
-- 2 = double blink
-- 4 = close your eyes
-- 8 = jump
-- 16 = reverse the order of the operations
handshake :: Int -> [String]
handshake n =
  let actions = [ (1, "wink")
                , (2, "double blink")
                , (4, "close your eyes")
                , (8, "jump")
                ]
      -- Build the list of actions based on the first 4 bits
      result = [ action | (bit, action) <- actions, n .&. bit /= 0 ]
      -- Check if the 5th bit (16) is set to reverse the order
      shouldReverse = n .&. 16 /= 0
  in if shouldReverse
     then reverse result
     else result
