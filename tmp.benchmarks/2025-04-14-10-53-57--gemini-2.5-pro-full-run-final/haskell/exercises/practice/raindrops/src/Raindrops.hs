module Raindrops (convert) where

-- | Converts a number to a raindrop sound string based on its factors.
-- If the number has 3 as a factor, append "Pling".
-- If the number has 5 as a factor, append "Plang".
-- If the number has 7 as a factor, append "Plong".
-- If the number does not have 3, 5, or 7 as a factor, return the number's string representation.
convert :: Int -> String
convert n =
  let factors = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
      sounds = concat [sound | (factor, sound) <- factors, n `mod` factor == 0]
  in if null sounds
     then show n
     else sounds
