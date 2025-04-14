module Raindrops (convert) where

convert :: Int -> String
convert n = if null result then show n else result
  where
    result = concat [if n `mod` x == 0 then sound else "" | (x, sound) <- [(3, "Pling"), (5, "Plang"), (7, "Plong")]]
