module SecretHandshake (handshake) where

import Data.Bits ((.&.))

-- | Convert an integer (considering only its five least‑significant bits)
--   into the corresponding secret‑handshake actions.
--   The bits represent, from least significant to most significant:
--     0 (1)  -> "wink"
--     1 (2)  -> "double blink"
--     2 (4)  -> "close your eyes"
--     3 (8)  -> "jump"
--     4 (16) -> reverse the accumulated actions
handshake :: Int -> [String]
handshake n = applyReverseIfNeeded basicActions
  where
    -- Build the list of actions in the prescribed order.
    basicActions =
      concat
        [ if n .&. 1  /= 0 then ["wink"]          else []
        , if n .&. 2  /= 0 then ["double blink"]  else []
        , if n .&. 4  /= 0 then ["close your eyes"] else []
        , if n .&. 8  /= 0 then ["jump"]          else []
        ]

    -- If the 5th bit (value 16) is set, reverse the sequence.
    applyReverseIfNeeded actions
      | n .&. 16 /= 0 = reverse actions
      | otherwise     = actions
