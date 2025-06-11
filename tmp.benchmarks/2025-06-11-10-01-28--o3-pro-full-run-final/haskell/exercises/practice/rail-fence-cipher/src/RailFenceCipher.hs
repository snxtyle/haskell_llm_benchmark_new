module RailFenceCipher (encode, decode) where

import Data.List (sortBy)
import Data.Ord  (comparing)

-- | Produce the repeating rail pattern for a given number of rails.
--   For example, for n = 3 the finite core pattern is [0,1,2,1]
railPattern :: Int -> [Int]
railPattern n
  | n <= 1    = repeat 0
  | otherwise = cycle $ asc ++ desc
  where
    asc  = [0 .. n - 1]
    desc = [n - 2, n - 3 .. 1]

-- | Split a list according to a list of lengths.
--   Each integer in the first argument describes the length of
--   the corresponding fragment in the result.
splitBy :: [Int] -> [a] -> [[a]]
splitBy [] _          = []
splitBy (k:ks) xs     = h : splitBy ks t
  where
    (h, t) = splitAt k xs

-- | Encode a plaintext message using the Rail Fence cipher.
encode :: Int -> String -> String
encode n txt
  | n <= 1    = txt
  | otherwise = concat rails
  where
    pattern = take (length txt) (railPattern n)
    rails   = [ [c | (c, idx) <- zip txt pattern, idx == r]
              | r <- [0 .. n - 1]
              ]

-- | Decode a ciphertext message that was encoded with the Rail Fence cipher.
decode :: Int -> String -> String
decode n cipher
  | n <= 1    = cipher
  | otherwise = map snd sortedByPos
  where
    len      = length cipher
    pattern  = take len (railPattern n)
    positions = zip [0 ..] pattern           -- (original position, rail number)

    -- Group positions by rail
    positionsByRail :: [[Int]]
    positionsByRail =
      [ [pos | (pos, rail) <- positions, rail == r]
      | r <- [0 .. n - 1]
      ]

    counts = map length positionsByRail

    -- Slice the ciphertext into segments for each rail.
    segments :: [String]
    segments = splitBy counts cipher

    -- Associate each character with its original position.
    paired :: [(Int, Char)]
    paired = concat $ zipWith zip positionsByRail segments

    -- Re-order by the original position to reconstruct the plaintext.
    sortedByPos :: [(Int, Char)]
    sortedByPos = sortBy (comparing fst) paired
