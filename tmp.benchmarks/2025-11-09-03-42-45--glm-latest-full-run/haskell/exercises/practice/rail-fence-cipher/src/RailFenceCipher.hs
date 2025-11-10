module RailFenceCipher (encode, decode) where

import Data.List (sortBy, groupBy, mapAccumL, sort, uncons)
import Data.Function (on)

-- Helper to generate the repeating rail index pattern
railPattern :: Int -> [Int]
railPattern n
    | n <= 1 = repeat 0
    | otherwise = cycle $ [0..n-1] ++ [n-2, n-1..1]

encode :: Int -> String -> String
encode n msg
    | n <= 1 = msg
    | null msg = ""
    | otherwise =
        let pattern = take (length msg) (railPattern n)
            zipped = zip msg pattern
            -- For each rail index i, filter the zipped list to find all characters
            -- belonging to that rail, preserving their original order.
            rails = [map fst $ filter (\p -> snd p == i) zipped | i <- [0..n-1]]
        in concat rails

-- Helper to get the length of each rail in the encoded message
getRailLengths :: Int -> Int -> [Int]
getRailLengths n len
    | n <= 1 = [len]
    | otherwise = map length $ groupBy ((==) `on` id) $ sort $ take len (railPattern n)

decode :: Int -> String -> String
decode n c
    | n <= 1 = c
    | null c = ""
    | otherwise =
        let len = length c
            pattern = take len (railPattern n)
            railLengths = getRailLengths n len
            
            -- Split the ciphertext into chunks for each rail
            splitIntoChunks _ [] = []
            splitIntoChunks [] s = [s]
            splitIntoChunks (l:ls) s = let (chunk, rest) = splitAt l s in chunk : splitIntoChunks ls rest
            rails = splitIntoChunks railLengths c
            
            -- Reconstruct the message by following the pattern and pulling chars from rails
            -- mapAccumL traverses the pattern, carrying the list of rails as state.
            -- For each rail index 'p' in the pattern, it takes the head char from the p-th rail,
            -- updates the state by removing that char, and accumulates the char into the result.
            -- We use 'uncons' to safely handle the character extraction and silence a compiler warning.
            (_, message) = mapAccumL (\rs p ->
                                         case uncons (rs !! p) of
                                             Just (char, rest) ->
                                                 let newRs = take p rs ++ [rest] ++ drop (p + 1) rs
                                                 in (newRs, char)
                                             -- This case should not be reachable with valid inputs,
                                             -- but is required to make the pattern match total.
                                             Nothing -> error "Unexpected empty rail during decoding") rails pattern
        in message
