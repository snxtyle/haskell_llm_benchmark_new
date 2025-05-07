module Matrix (saddlePoints) where

import Data.Array (Array, (!), bounds, array)

-- | Restituisce tutte le coordinate (riga,colonna) che sono
--   massimi nella propria riga e minimi nella propria colonna.
saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix =
  [ (r, c)
  | r <- rows
  , c <- cols
  , let val = matrix ! (r, c)
  , val == rowMax ! r
  , val == colMin ! c
  ]
  where
    -- Estremi della matrice
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    rows = [minRow .. maxRow]
    cols = [minCol .. maxCol]

    -- Massimo per ogni riga
    rowMax = array (minRow, maxRow)
             [ (r, maximum [matrix ! (r, c) | c <- cols]) | r <- rows ]

    -- Minimo per ogni colonna
    colMin = array (minCol, maxCol)
             [ (c, minimum [matrix ! (r, c) | r <- rows]) | c <- cols ]
