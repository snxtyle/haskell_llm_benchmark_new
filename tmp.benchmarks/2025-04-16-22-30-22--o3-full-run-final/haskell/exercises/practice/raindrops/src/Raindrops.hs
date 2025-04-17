module Raindrops (convert) where

-- | Convert an integer to its corresponding raindrop sound.
--   • If the number is divisible by 3, add "Pling".
--   • If the number is divisible by 5, add "Plang".
--   • If the number is divisible by 7, add "Plong".
--   • If none apply, return the number itself.
convert :: Int -> String
convert n =
  let sound =
        concat
          [ if n `mod` 3 == 0 then "Pling" else ""
          , if n `mod` 5 == 0 then "Plang" else ""
          , if n `mod` 7 == 0 then "Plong" else ""
          ]
   in if null sound
        then show n
        else sound
