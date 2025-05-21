module SecretHandshake (handshake) where

import Data.Bits (testBit)
import Data.List (reverse)

handshake :: Int -> [String]
handshake n =
    let
        -- Collect actions based on bits 0, 1, 2, 3 in order.
        -- The `++` operator concatenates lists.
        actions =
            (if testBit n 0 then ["wink"] else []) ++
            (if testBit n 1 then ["double blink"] else []) ++
            (if testBit n 2 then ["close your eyes"] else []) ++
            (if testBit n 3 then ["jump"] else [])

        -- Check bit 4 to determine if the order of actions should be reversed.
        finalActions = if testBit n 4 then Data.List.reverse actions else actions
    in
        finalActions
