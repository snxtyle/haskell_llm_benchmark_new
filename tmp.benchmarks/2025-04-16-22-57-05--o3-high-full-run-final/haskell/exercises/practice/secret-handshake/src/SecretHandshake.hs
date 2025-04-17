module SecretHandshake (handshake) where

import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n
  | n <= 0    = []
  | otherwise = applyReverseFlag selected
  where
    actions :: [(Int, String)]
    actions =
      [ (1,  "wink")
      , (2,  "double blink")
      , (4,  "close your eyes")
      , (8,  "jump")
      ]

    selected :: [String]
    selected = [action | (bit, action) <- actions, n .&. bit /= 0]

    applyReverseFlag :: [String] -> [String]
    applyReverseFlag xs
      | n .&. 16 /= 0 = reverse xs
      | otherwise     = xs
