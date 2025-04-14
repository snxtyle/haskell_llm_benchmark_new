module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = 
    let actions = buildActions (n `mod` 32) -- Only consider last 5 bits (mod 32)
    in if shouldReverse n then reverse actions else actions
  where
    buildActions num = concat $ zipWith (\bit action -> if bit == 1 then [action] else [])
                                        (map (bitTest num) [0..3])
                                        ["wink", "double blink", "close your eyes", "jump"]
    bitTest num pos = if num `mod` (2 ^ (pos + 1)) >= 2 ^ pos then 1 else 0
    shouldReverse num = bitTest num 4 == 1
