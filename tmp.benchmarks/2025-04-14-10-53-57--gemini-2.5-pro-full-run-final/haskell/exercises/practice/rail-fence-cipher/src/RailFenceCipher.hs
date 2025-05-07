module RailFenceCipher (encode, decode) where

import Data.List (sortOn, zipWith)

-- Generates the sequence of rail indices (0-based) in a zig-zag pattern.
-- Example: railIndices 3 10 -> [0, 1, 2, 1, 0, 1, 2, 1, 0, 1]
railIndices :: Int -> Int -> [Int]
railIndices n _ | n <= 1 = repeat 0
railIndices n len = take len $ cycle $ [0 .. n - 1] ++ [n - 2, n - 3 .. 1]

-- Encodes a string using the Rail Fence Cipher.
encode :: Int -> String -> String
encode n s
  | n <= 0 = error "Number of rails must be positive"
  | n == 1 = s
  | otherwise =
      let len = length s
          indices = railIndices n len
          -- Pair each character with its rail index: [(Char, Int)]
          charWithIndex = zip s indices
          -- Sort pairs based on rail index: [(Char, Int)]
          sortedChars = sortOn snd charWithIndex
       in -- Extract characters in sorted order
          map fst sortedChars

-- Decodes a string encoded with the Rail Fence Cipher.
decode :: Int -> String -> String
decode n s
  | n <= 0 = error "Number of rails must be positive"
  | n == 1 = s
  | otherwise =
      let len = length s
          indices = railIndices n len
          -- Pair original position index with rail index: [(Int, Int)]
          posWithIndex = zip [0 .. len - 1] indices
          -- Sort based on rail index, then original position: [(Int, Int)]
          -- This gives the order characters appear in the *encoded* string
          sortedPos = sortOn snd posWithIndex
          -- Pair the characters from the encoded string with their original positions
          -- [(Char, (Int, Int))] where inner tuple is (original_pos, rail_index)
          charWithPos = zip s sortedPos
          -- Sort back based on the original position index
          -- [(Char, (Int, Int))]
          sortedByOriginalPos = sortOn (fst . snd) charWithPos
       in -- Extract characters in their original order
          map fst sortedByOriginalPos

