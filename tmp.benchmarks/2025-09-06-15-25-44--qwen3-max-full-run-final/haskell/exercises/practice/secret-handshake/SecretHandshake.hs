module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = processBits (n .&. 31)
  where
    -- Extract bits and build the handshake
    processBits num = applyReverse (num .&. 16) $ buildHandshake num
    
    -- Build handshake from bits 0-3
    buildHandshake num = 
      [ if num .&. 1 /= 0 then "wink" else "",
        if num .&. 2 /= 0 then "double blink" else "",
        if num .&. 4 /= 0 then "close your eyes" else "",
        if num .&. 8 /= 0 then "jump" else ""
      ] |> filter (not . null)
    
    -- Apply reverse if bit 4 is set
    applyReverse 0 actions = actions
    applyReverse _ actions = reverse actions
    
    -- Helper function for piping
    (|>) x f = f x

-- Bitwise AND operation
(.&.) :: Int -> Int -> Int
(.&.) a b = sum [2^i | i <- [0..31], testBit a i, testBit b i]
  where
    testBit x i = (x `div` (2^i)) `mod` 2 == 1
