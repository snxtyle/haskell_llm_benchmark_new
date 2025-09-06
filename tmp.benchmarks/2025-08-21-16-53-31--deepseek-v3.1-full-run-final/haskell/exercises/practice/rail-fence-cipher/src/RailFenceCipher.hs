module RailFenceCipher (encode, decode) where

import Data.List (sortOn, groupBy)
import Data.Function (on)

encode :: Int -> String -> String
encode rails message = concat [map snd row | row <- rows]
  where
    -- Generate the zig-zag pattern
    pattern = take (length message) $ cycle ([0..rails-1] ++ reverse [1..rails-2])
    -- Pair characters with their row numbers
    withRows = zip pattern message
    -- Group by row number and sort each group
    rows = map (map snd) $ groupBy ((==) `on` fst) $ sortOn fst withRows

decode :: Int -> String -> String
decode rails cipher = map snd $ sortOn fst $ zip [0..len-1] cipherInOrder
  where
    len = length cipher
    -- Generate the zig-zag pattern
    pattern = take len $ cycle ([0..rails-1] ++ reverse [1..rails-2])
    
    -- Create a list of (row, index) pairs and sort by row
    sortedByRow = sortOn fst $ zip pattern [0..len-1]
    
    -- Pair cipher characters with their original positions
    cipherWithPos = zip (map snd sortedByRow) cipher
    
    -- Sort by original position to get cipher in the right order
    cipherInOrder = map snd $ sortOn fst cipherWithPos
