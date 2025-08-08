module Pangram (isPangram) where

import Data.Bits ((.|.), shiftL)
import Data.Char (toLower, ord)
import Data.List (foldl')

isPangram :: String -> Bool
isPangram text = mask == fullMask
  where
    mask = foldl' step 0 text
    fullMask = (1 `shiftL` 26) - 1

    step :: Int -> Char -> Int
    step acc ch
      | 'a' <= lc && lc <= 'z' =
          acc .|. (1 `shiftL` (ord lc - ord 'a'))
      | otherwise = acc
      where
        lc = toLower ch
