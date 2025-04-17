module RailFenceCipher (encode, decode) where

import Data.List (mapAccumL)

encode :: Int -> String -> String
encode rails text
    | rails <= 1 = text
    | otherwise  = concat [ [ c | (c,i) <- zip text idxs, i == r ] | r <- [0..rails-1] ]
  where
    len    = length text
    period = 2 * (rails - 1)
    idxs   = [ let m = k `mod` period in if m < rails then m else period - m
             | k <- [0 .. len - 1] ]

decode :: Int -> String -> String
decode rails text
    | rails <= 1 = text
    | otherwise  = result
  where
    len      = length text
    period   = 2 * (rails - 1)
    idxs     = [ let m = k `mod` period in if m < rails then m else period - m
               | k <- [0 .. len - 1] ]
    counts   = [ length (filter (== r) idxs) | r <- [0..rails-1] ]
    segments = splitCounts counts text
    (_, result) = mapAccumL pick segments idxs

    pick segs r =
      let (before, cur:after) = splitAt r segs
          c:cs               = cur
      in (before ++ (cs : after), c)

splitCounts :: [Int] -> [a] -> [[a]]
splitCounts [] _       = []
splitCounts (n:ns) xs  = let (h, t) = splitAt n xs
                         in h : splitCounts ns t
