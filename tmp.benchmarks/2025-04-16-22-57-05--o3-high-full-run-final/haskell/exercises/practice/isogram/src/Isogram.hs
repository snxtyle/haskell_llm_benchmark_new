module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)
import qualified Data.List as List

-- | Determine if a word or phrase is an isogram.
--   An isogram is a string without any repeating letters.
--   Spaces and hyphens are ignored, and comparison is case‑insensitive.
isIsogram :: String -> Bool
isIsogram xs =
  let letters = map toLower (filter isAlpha xs)
  in length letters == length (List.nub letters)
