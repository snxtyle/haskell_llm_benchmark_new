module Matrix (saddlePoints) where

import Data.Array (Array, (!), bounds, array)

saddlePoints :: Ord a => Array (Int, Int) a -> [(Int, Int)]
saddlePoints matrix
  | rMin > rMax || cMin > cMax = []
  | otherwise =
      let rows = [rMin .. rMax]
          cols = [cMin .. cMax]
          rowMaxes = array (rMin, rMax)
                        [ (r, maximum [matrix ! (r, c) | c <- cols])
                        | r <- rows
                        ]
          colMins = array (cMin, cMax)
                        [ (c, minimum [matrix ! (r, c) | r <- rows])
                        | c <- cols
                        ]
      in [ (r, c)
         | r <- rows
         , c <- cols
         , let val = matrix ! (r, c)
         , val == rowMaxes ! r
         , val == colMins ! c
         ]
  where
    ((rMin, cMin), (rMax, cMax)) = bounds matrix
