module Raindrops (convert) where

-- | Convert a number to its corresponding raindrop sound.
--
--   * If the number is divisible by 3, include "Pling".
--   * If the number is divisible by 5, include "Plang".
--   * If the number is divisible by 7, include "Plong".
--   * If it is not divisible by 3, 5, or 7, return the number itself.
--
--   Examples:
--
--   >>> convert 28
--   "Plong"
--
--   >>> convert 30
--   "PlingPlang"
--
--   >>> convert 34
--   "34"
convert :: Int -> String
convert n
  | null sound = show n
  | otherwise  = sound
  where
    sound = concat $ [ "Pling" | n `mod` 3 == 0 ]
                  ++ [ "Plang" | n `mod` 5 == 0 ]
                  ++ [ "Plong" | n `mod` 7 == 0 ]
