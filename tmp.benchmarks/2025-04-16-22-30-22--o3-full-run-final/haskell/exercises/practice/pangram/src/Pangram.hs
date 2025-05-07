module Pangram (isPangram) where

import Data.Char (toLower)

-- | Determine whether a text is a pangram.
--   A pangram contains every letter from 'a' to 'z' at least once,
--   ignoring case and any non‑alphabetic characters.
isPangram :: String -> Bool
isPangram text = all (`elem` normalized) ['a' .. 'z']
  where
    normalized = map toLower text
