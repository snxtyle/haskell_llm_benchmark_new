module Pangram (isPangram) where

import Data.Bits ((.|.), shiftL)
import Data.Char (toLower, ord)

isPangram :: String -> Bool
isPangram text = seenMask == fullMask
  where
    fullMask :: Int
    fullMask = (1 `shiftL` 26) - 1

    seenMask :: Int
    seenMask = foldl update 0 text

    update :: Int -> Char -> Int
    update acc ch =
      let c = toLower ch
      in if c >= 'a' && c <= 'z'
           then acc .|. (1 `shiftL` (ord c - ord 'a'))
           else acc
