module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, indices, (!))

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = 
    [ (r, c) | (r, c) <- indices matrix
             , let value = matrix ! (r, c)
             , isMaxInRow value r
             , isMinInColumn value c
    ]
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    
    -- Check if value is maximum in its row
    isMaxInRow value row = all (<= value) [matrix ! (row, c) | c <- [minCol..maxCol]]
    
    -- Check if value is minimum in its column  
    isMinInColumn value col = all (>= value) [matrix ! (r, col) | r <- [minRow..maxRow]]
