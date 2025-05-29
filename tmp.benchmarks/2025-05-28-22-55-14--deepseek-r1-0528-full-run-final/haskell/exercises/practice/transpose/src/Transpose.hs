module Transpose (transpose) where

transpose :: [String] -> [String]
transpose ls = [ processColumn j | j <- [0..maxCol-1] ]
  where
    n = length ls
    maxCol = if n == 0 then 0 else maximum (map length ls)
    processColumn j = 
        let m = maximum [ i | i <- [0..n-1], j < length (ls !! i) ]
        in [ charAt i j | i <- [0..m] ]
    charAt i j
        | j < length (ls !! i) = (ls !! i) !! j
        | otherwise = ' '
