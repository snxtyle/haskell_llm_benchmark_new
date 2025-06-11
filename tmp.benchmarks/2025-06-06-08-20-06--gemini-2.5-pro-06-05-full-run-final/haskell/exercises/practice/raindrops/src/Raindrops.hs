module Raindrops (convert) where

convert :: Int -> String
convert n =
    let drops = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
        sounds = concat [sound | (factor, sound) <- drops, n `mod` factor == 0]
    in if null sounds
       then show n
       else sounds
