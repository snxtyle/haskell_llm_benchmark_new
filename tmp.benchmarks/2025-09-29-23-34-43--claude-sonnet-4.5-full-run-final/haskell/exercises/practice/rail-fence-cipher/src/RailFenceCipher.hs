module RailFenceCipher (encode, decode) where

import Data.List (sortBy)
import Data.Ord (comparing)

encode :: Int -> String -> String
encode numRails plaintext
  | numRails <= 0 = plaintext
  | numRails == 1 = plaintext
  | otherwise = map snd $ sortBy (comparing fst) indexed
  where
    -- Pair each character with its (rail, position) coordinates
    indexed = zipWith (\char (rail, pos) -> ((rail, pos), char)) plaintext coords
    
    -- Generate coordinates for the zig-zag pattern
    coords = zip (railPattern numRails) [0..]
    
    -- Generate the rail pattern (which rail each position goes to)
    railPattern n = cycle (upDown n)
    
    -- Create one complete zig-zag cycle: 0,1,2,...,n-1,n-2,...,1
    upDown n = [0..n-1] ++ [n-2,n-3..1]

decode :: Int -> String -> String
decode numRails ciphertext
  | numRails <= 0 = ciphertext
  | numRails == 1 = ciphertext
  | otherwise = map snd $ sortBy (comparing fst) filledPositions
  where
    len = length ciphertext
    
    -- Generate the zig-zag pattern for positions
    zigzagPattern = take len $ railPattern numRails
    
    -- Create (rail, column) pairs for each position
    positionsWithRails = zip zigzagPattern [0..]
    
    -- Sort by rail first, then by column to get reading order
    sortedPositions = sortBy (comparing (\(r, c) -> (r, c))) positionsWithRails
    
    -- Pair each position with its character from ciphertext
    filledPositions = zipWith (\char (rail, col) -> (col, char)) ciphertext sortedPositions
    
    -- Generate the rail pattern (which rail each position goes to)
    railPattern n = cycle (upDown n)
    
    -- Create one complete zig-zag cycle: 0,1,2,...,n-1,n-2,...,1
    upDown n = [0..n-1] ++ [n-2,n-3..1]
