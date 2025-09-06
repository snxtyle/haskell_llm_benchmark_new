module Spiral (spiral) where

import Data.Array

spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise = [[arr ! (i, j) | j <- 1 .. n] | i <- 1 .. n]
where
  arr = array ((1, 1), (n, n)) (spiralFill n)

spiralFill :: Int -> [((Int, Int), Int)]
spiralFill n = zip positions [1 .. n * n]
  where
    positions = concat [layerPositions layer n | layer <- [0 .. (n - 1) `div` 2]]

    layerPositions :: Int -> Int -> [(Int, Int)]
    layerPositions layer n =
      let topR = layer + 1
          botR = n - layer
          lefC = layer + 1
          rigC = n - layer
      in
        -- Top row
        [(topR, c) | c <- [lefC .. rigC]] ++
        -- Right column
        [(r, rigC) | r <- [topR + 1 .. botR]] ++
        -- Bottom row
        [(botR, c) | c <- [rigC - 1, rigC - 2 .. lefC]] ++
        -- Left column
        [(r, lefC) | r <- [botR - 1, botR - 2 .. topR + 1]]
