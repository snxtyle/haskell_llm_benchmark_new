module SecretHandshake (handshake) where

import Data.Bits ((.&.)) -- Import the bitwise AND operator

-- Define the mapping from bit values to actions
actions :: [(Int, String)]
actions = [ (1, "wink")          -- 2^0
          , (2, "double blink")   -- 2^1
          , (4, "close your eyes")-- 2^2
          , (8, "jump")           -- 2^3
          ]

-- Define the bit value for the reverse operation
reverseBit :: Int
reverseBit = 16 -- 2^4

-- Function to convert a number to a secret handshake sequence
handshake :: Int -> [String]
handshake n =
    -- Generate the initial list of actions by checking bits 0 through 3
    let initialActions = [ action | (bitValue, action) <- actions, n .&. bitValue /= 0 ]
        -- Check if the reverse bit (bit 4) is set
        shouldReverse = n .&. reverseBit /= 0
    -- Return the actions, reversed if necessary
    in if shouldReverse
       then reverse initialActions
       else initialActions
