module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

encode :: Int -> String -> String
encode rails text
  | rails <= 0 || null text = ""
  | rails == 1 = text
  | otherwise = map snd $ sortOn fst $ zip (railPattern rails (length text)) text

decode :: Int -> String -> String
decode rails cipher
  | rails <= 0 || null cipher = ""
  | rails == 1 = cipher
  | otherwise = map snd $ sortOn fst $ zip indices cipher
  where
    pattern = railPattern rails (length cipher)
    indices = map snd $ sortOn fst $ zip pattern [0..]

-- Generate the rail pattern for a given number of rails and message length
railPattern :: Int -> Int -> [Int]
railPattern rails len = take len $ cycle zigzag
  where
    zigzag = [0..rails-1] ++ [rails-2, rails-3..1]
