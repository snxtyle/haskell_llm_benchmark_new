module Raindrops (convert) where

convert :: Int -> String
convert n
  | null sounds = show n
  | otherwise = concat sounds
  where
    sounds = [sound | (divisor, sound) <- [(3, "Pling"), (5, "Plang"), (7, "Plong")], n `mod` divisor == 0]
