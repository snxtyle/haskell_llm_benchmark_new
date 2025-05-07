module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = let
    actions = ["wink", "double blink", "close your eyes", "jump"]
    bits = map (testBit n) [0..3]
    selected = [a | (a, True) <- zip actions bits]
    reversed = testBit n 4
  in if reversed then reverse selected else selected

-- Helper function to test if a bit is set at a given position
testBit :: Int -> Int -> Bool
testBit num pos = (num `div` (2 ^ pos)) `mod` 2 == 1
