module RailFenceCipher (encode, decode) where

import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Function (on)

-- Helper: generate the rail pattern for a given number of rails and message length
railPattern :: Int -> Int -> [Int]
railPattern rails len
  | rails <= 1 = replicate len 0
  | otherwise  = take len $ cycle (down ++ up)
  where
    down = [0 .. rails - 1]
    up   = [rails - 2, rails - 3 .. 1]

-- ENCODE
encode :: Int -> String -> String
encode rails msg =
  concat [ [c | (r, c) <- zipped, r == rail] | rail <- [0 .. rails - 1] ]
  where
    pattern = railPattern rails (length msg)
    zipped = zip pattern msg

-- DECODE
decode :: Int -> String -> String
decode rails cipher =
  map snd $ sortOn fst $ concat railsWithChars
  where
    len = length cipher
    pattern = railPattern rails len
    -- For each rail, get the indices in the message that belong to that rail
    railIndices = [ [i | (i, r) <- zip [0..] pattern, r == rail] | rail <- [0 .. rails - 1] ]
    -- For each rail, take the corresponding number of chars from the cipher
    charsPerRail = scanl (+) 0 (map length railIndices)
    railsWithChars = [ zip idxs (take (length idxs) (drop (charsPerRail !! rail) cipher))
                     | (rail, idxs) <- zip [0..] railIndices ]
