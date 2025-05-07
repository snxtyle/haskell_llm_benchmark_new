module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n
  | length digits <= 1 = False
  | any (not . isDigit) filtered = False
  | otherwise = total `mod` 10 == 0
  where
    filtered = filter (/= ' ') n
    digits = filtered
    values = map digitToInt digits
    -- Double every second digit from the right
    doubled = reverse $ zipWith ($) (cycle [id, luhnDouble]) (reverse values)
    total = sum doubled

    luhnDouble x = let d = x * 2 in if d > 9 then d - 9 else d
