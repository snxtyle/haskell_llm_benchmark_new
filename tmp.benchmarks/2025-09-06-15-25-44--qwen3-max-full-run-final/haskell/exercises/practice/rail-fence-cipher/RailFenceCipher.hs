module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

encode :: Int -> String -> String
encode rails text = map snd $ sortOn fst $ zip (railIndices rails (length text)) text

decode :: Int -> String -> String
decode rails ciphertext = map snd $ sortOn fst $ zip (originalPositions rails (length ciphertext)) (zip [0..] ciphertext)
  where
    originalPositions r len = map fst $ sortOn snd $ zip [0..] (railIndices r len)

-- Generate the rail index for each position in the text
railIndices :: Int -> Int -> [Int]
railIndices rails len = take len $ cycle (down ++ up)
  where
    down = [0..rails-1]
    up = if rails > 2 then reverse [1..rails-2] else []
