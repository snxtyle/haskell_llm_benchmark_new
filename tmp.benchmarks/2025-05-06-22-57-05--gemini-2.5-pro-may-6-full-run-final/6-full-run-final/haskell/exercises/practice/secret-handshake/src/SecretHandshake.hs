module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n =
    let
        -- Determine actions based on bits 0-3.
        -- The order here respects "Start at the right-most digit and move left."
        -- meaning bit 0's action comes before bit 1's action, etc.
        actions_in_order =
            (if testBit n 0 then ["wink"] else []) ++
            (if testBit n 1 then ["double blink"] else []) ++
            (if testBit n 2 then ["close your eyes"] else []) ++
            (if testBit n 3 then ["jump"] else [])

        -- Check if bit 4 is set, which means we need to reverse the order of operations.
        should_reverse = testBit n 4

        final_actions = if should_reverse
                        then reverse actions_in_order
                        else actions_in_order
    in
        final_actions
