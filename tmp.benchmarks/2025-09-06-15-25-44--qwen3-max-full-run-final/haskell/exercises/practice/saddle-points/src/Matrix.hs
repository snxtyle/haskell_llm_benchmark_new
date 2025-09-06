module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), indices, assocs)
import Data.Ix (range)

saddlePoints :: (Ord e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = 
    let ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
        
        -- Get all row indices and column indices
        rows = [minRow..maxRow]
        cols = [minCol..maxCol]
        
        -- For each row, find the maximum value and its positions
        rowMaxes = [(r, c) | r <- rows, c <- cols, 
                    let rowValues = [matrix ! (r, c') | c' <- cols],
                    matrix ! (r, c) == maximum rowValues]
        
        -- For each column, find the minimum value and its positions
        colMins = [(r, c) | r <- rows, c <- cols,
                    let colValues = [matrix ! (r', c) | r' <- rows],
                    matrix ! (r, c) == minimum colValues]
        
        -- Find intersection - points that are both row maxima and column minima
        saddlePoints = filter (\pos -> pos `elem` colMins) rowMaxes
        
    in saddlePoints
