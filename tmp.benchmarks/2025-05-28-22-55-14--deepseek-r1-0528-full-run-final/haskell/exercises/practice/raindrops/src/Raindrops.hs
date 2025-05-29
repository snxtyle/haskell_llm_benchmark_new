module Raindrops (convert) where

convert :: Int -> String
convert n 
  | null soundString = show n
  | otherwise = soundString
  where
    factors = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    soundString = concat [sound | (factor, sound) <- factors, n `mod` factor == 0]
