module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

-- Generates the pattern of rail indices for a given number of rails.
-- e.g., for 3 rails: [0, 1, 2, 1]
-- This formula works for n >= 1.
-- For n=1, it's [0..0] ++ reverse [1..-1] -> [0] ++ [] -> [0].
-- For n=2, it's [0..1] ++ reverse [1..0] -> [0,1] ++ [] -> [0,1].
railsPattern :: Int -> [Int]
railsPattern n = [0..n-1] ++ reverse [1..n-2]

encode :: Int -> String -> String
encode n text
  | n <= 1 = text
  | otherwise =
      let len = length text
          -- Generate the rail index for each character in the original text
          railIndices = take len . cycle $ railsPattern n
          -- Pair each character with its rail index
          indexedChars = zip text railIndices
          -- Sort by rail index. This groups characters on the same rail together.
          -- `sortOn` is stable, preserving original order for characters on the same rail.
          sortedByRail = sortOn snd indexedChars
      in map fst sortedByRail

decode :: Int -> String -> String
decode n text
  | n <= 1 = text
  | otherwise =
      let len = length text
          -- Generate the rail index for each character in the original text
          railIndices = take len . cycle $ railsPattern n
          -- Create pairs of (rail index, original position)
          -- e.g. for n=3: [(0,0), (1,1), (2,2), (1,3), (0,4), ...]
          positions = zip railIndices [0..]
          -- Sort by rail index to find out which original positions belong to which rail.
          -- This gives the order of characters as they appear in the encoded string.
          -- e.g. for n=3: [(0,0), (0,4), ..., (1,1), (1,3), ..., (2,2), ...]
          encodedOrder = sortOn fst positions
          -- Pair the characters from the encoded text with their original positions.
          -- The `snd` of each pair in `encodedOrder` is an original position index.
          charsWithOriginalPos = zip text (map snd encodedOrder)
          -- Sort by the original position to restore the message.
          sortedByOriginalPos = sortOn snd charsWithOriginalPos
      in map fst sortedByOriginalPos
