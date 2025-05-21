module RailFenceCipher (encode, decode) where

import Data.List (sortBy, groupBy, sortOn)
import Data.Function (on)

-- Generates a single cycle of rail indices: 0, 1, ..., numRails-1, numRails-2, ..., 1
-- Examples:
-- railCycle 1 -> [0]
-- railCycle 2 -> [0, 1, 0]
-- railCycle 3 -> [0, 1, 2, 1]
-- railCycle 4 -> [0, 1, 2, 3, 2, 1]
railCycle :: Int -> [Int]
railCycle numRails
    | numRails <= 1 = [0]
    | otherwise     = [0 .. numRails - 1] ++ [numRails - 2, numRails - 3 .. 1]

-- Helper to generate the infinite zig-zag pattern of rail indices
railPattern :: Int -> [Int]
railPattern numRails = cycle (railCycle numRails)

encode :: Int -> String -> String
encode numRails text
    | null text     = ""
    | numRails <= 0 = error "Number of rails must be positive"
    | otherwise     =
        let
            -- Pair each character with its rail index
            -- Take only as many rail indices as there are characters in the text
            indexedChars = zip (take (length text) (railPattern numRails)) text
            -- Group characters by their rail index.
            -- sortBy (compare `on` fst) ensures groups are ordered by rail index.
            -- groupBy ((==) `on` fst) groups consecutive elements with the same rail index.
            groupedByRail = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) indexedChars
            -- Extract characters from each group and concatenate them.
            -- The order of groups is already by rail index (0, 1, ..., numRails-1).
            result = concatMap (map snd) groupedByRail
        in
            result

decode :: Int -> String -> String
decode numRails cipherText
    | null cipherText = ""
    | numRails <= 0   = error "Number of rails must be positive"
    | otherwise       =
        let
            len = length cipherText
            -- Generate (original_index, rail_index) pairs for the plaintext shape.
            -- This represents where each character *would* be in the plaintext grid.
            indexedPlaintextPositions = zip [0 .. len - 1] (take len (railPattern numRails))

            -- Sort these positions by their rail index.
            -- This groups all positions belonging to rail 0, then rail 1, etc.
            -- Example for len=7, rails=3: [(0,0), (4,0), (1,1), (3,1), (5,1), (2,2), (6,2)]
            sortedByRail = sortOn snd indexedPlaintextPositions

            -- Group the sorted positions by rail index.
            -- Example: [[(0,0), (4,0)], [(1,1), (3,1), (5,1)], [(2,2), (6,2)]]
            positionsByRail = groupBy ((==) `on` snd) sortedByRail

            -- Calculate the number of characters that belong to each rail.
            -- Example: [2, 3, 2]
            railLengths = map length positionsByRail

            -- Helper to split a list into segments of given lengths.
            -- Example: splitList "WCTRELE" [2,3,2] -> ["WC", "TRE", "LE"]
            splitList :: String -> [Int] -> [String]
            splitList _ [] = []
            splitList cs (l:ls) = take l cs : splitList (drop l cs) ls

            -- Split the ciphertext into segments, each corresponding to a rail.
            -- The ciphertext is already ordered by rail (all rail 0 chars, then all rail 1 chars, etc.).
            splitCipherText = splitList cipherText railLengths

            -- Pair the original plaintext positions (sorted by rail) with the characters from the split ciphertext.
            -- `zipWith` applies a function to corresponding elements of two lists.
            -- For each rail's list of positions and its corresponding ciphertext segment:
            --   `zip (map fst positions) chars` creates `(original_index, char)` pairs.
            -- `concat` flattens the list of lists of pairs into a single list.
            -- Example: `[(0,'W'), (4,'E'), (1,'C'), (3,'R'), (5,'L'), (2,'T'), (6,'E')]`
            zippedPairs = concat $ zipWith (\positions chars -> zip (map fst positions) chars) positionsByRail splitCipherText

            -- Sort these (original_index, char) pairs by their original index.
            -- This reconstructs the plaintext in its correct order.
            -- Example: `[(0,'W'), (1,'C'), (2,'T'), (3,'R'), (4,'E'), (5,'L'), (6,'E')]`
            finalSorted = sortOn fst zippedPairs

            -- Extract the characters to get the final plaintext.
            result = map snd finalSorted
        in
            result
