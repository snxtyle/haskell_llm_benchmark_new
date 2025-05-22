module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = if shouldReverse then reverse actions else actions
  where
    actions = [action | (bit, action) <- zip [1..4] ["wink", "double blink", "close your eyes", "jump"], n `testBit` (bit - 1)]
    shouldReverse = n `testBit` 4
    testBit x bit = (x `div` (2^bit)) `mod` 2 == 1
