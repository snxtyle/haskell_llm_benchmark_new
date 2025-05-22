module RailFenceCipher (encode, decode) where

import Data.List (sortBy)
import Data.Function (on)

encode :: Int -> String -> String
encode rails message = map snd $ sortBy (compare `on` fst) indexedChars
  where
    indexedChars = zip (map getRail [0..]) message
    getRail pos = let cycle = 2 * (rails - 1)
                      posInCycle = pos `mod` cycle
                  in if posInCycle < rails
                     then posInCycle
                     else cycle - posInCycle

decode :: Int -> String -> String
decode rails encoded = map snd $ sortBy (compare `on` fst) $ zip positions encoded
  where
    messageLength = length encoded
    positions = concatMap (getRailPositions messageLength rails) [0..rails-1]

getRailPositions :: Int -> Int -> Int -> [Int]
getRailPositions messageLength rails rail = 
    filter (< messageLength) $ 
    if rails == 1 
    then [0..messageLength-1]
    else if rail == 0 || rail == rails - 1
         then [rail, rail + cycle..]
         else generateMiddleRailPositions rail cycle
  where
    cycle = 2 * (rails - 1)
    generateMiddleRailPositions r c = 
        let step1 = c - 2 * r
            step2 = 2 * r
            positions = r : concatMap (\n -> [r + n * c + step1, r + (n + 1) * c]) [0..]
        in positions
