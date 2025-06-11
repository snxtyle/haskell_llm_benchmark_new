module SecretHandshake (handshake) where

import Data.Bits (testBit)

-- | Convert an integer into the corresponding secret-handshake actions.
--
-- The right-most 5 bits of the number determine the actions:
--
-- Bit 0 (1):  "wink"
-- Bit 1 (2):  "double blink"
-- Bit 2 (4):  "close your eyes"
-- Bit 3 (8):  "jump"
-- Bit 4 (16): If set, reverse the order of the previous actions.
--
-- Any bits to the left of bit 4 are ignored. Numbers outside the 0-31
-- range are wrapped into that range via `mod 32`, matching the exercise
-- specification of considering only the lowest five bits.
handshake :: Int -> [String]
handshake n = finalActions
  where
    m = n `mod` 32  -- only keep the least-significant 5 bits
    actions =
      [ (0, "wink")
      , (1, "double blink")
      , (2, "close your eyes")
      , (3, "jump")
      ]
    selected = [action | (bit, action) <- actions, testBit m bit]
    finalActions
      | testBit m 4 = reverse selected
      | otherwise   = selected
