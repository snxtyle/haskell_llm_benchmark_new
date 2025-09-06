module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!), assocs)
import Data.List (transpose)

saddlePoints :: (Ord e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = 
    let (minRow, minCol) = fst $ bounds matrix
        (maxRow, maxCol) = snd $ bounds matrix
        
        -- Get all rows
        rows = [[matrix ! (r, c) | c <- [minCol..maxCol]] | r <- [minRow..maxRow]]
        
        -- Get all columns
        cols = [[matrix ! (r, c) | r <- [minRow..maxRow]] | c <- [minCol..maxCol]]
        
        -- Find row maximums
        rowMaxIndices = [ [(r, c) | c <- [minCol..maxCol], matrix ! (r, c) == maximum (rows !! (r - minRow))] 
                        | r <- [minRow..maxRow] ]
        
        -- Find column minimums
        colMinIndices = [ [(r, c) | r <- [minRow..maxRow], matrix ! (r, c) == minimum (cols !! (c - minCol))] 
                        | c <- [minCol..maxCol] ]
        
        -- Flatten and find intersection
        rowMaxSet = concat rowMaxIndices
        colMinSet = concat colMinIndices
        
    in filter (`elem` colMinSet) rowMaxSet
