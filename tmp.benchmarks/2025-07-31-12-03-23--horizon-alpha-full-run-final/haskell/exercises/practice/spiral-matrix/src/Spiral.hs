module Spiral (spiral) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)

-- | Produce an n x n spiral matrix filled with 1..n^2 in clockwise order.
--   For size <= 0, returns [].
spiral :: Int -> [[Int]]
spiral n
  | n <= 0 = []
  | otherwise = runST (buildSpiral n)

buildSpiral :: Int -> ST s [[Int]]
buildSpiral n = do
  -- 0-based indexing for convenience
  arr <- newArray ((0, 0), (n - 1, n - 1)) (0 :: Int) :: ST s (STUArray s (Int, Int) Int)
  _ <- fillLayers arr n 0 0 (n - 1) (n - 1) 1
  -- Collect results row by row
  rows <- mapM (\i -> mapM (\j -> readArray arr (i, j)) [0 .. n - 1]) [0 .. n - 1]
  pure rows

-- Fill layers recursively.
-- Parameters:
--   arr: array to fill
--   n: size
--   top, left, bottom, right: current layer boundaries (inclusive)
--   k: next number to write
-- Returns the next number after filling current layer.
fillLayers
  :: STUArray s (Int, Int) Int
  -> Int
  -> Int -> Int -> Int -> Int
  -> Int
  -> ST s Int
fillLayers _ _ top left bottom right k
  | top > bottom || left > right = pure k
fillLayers arr _ top left bottom right k = do
  -- top row: (top, left) -> (top, right)
  k1 <- writeRange arr [(top, j) | j <- [left .. right]] k
  -- right column: (top+1, right) -> (bottom, right)
  k2 <- writeRange arr [(i, right) | i <- [top + 1 .. bottom]] k1
  if top < bottom && left < right
    then do
      -- bottom row: (bottom, right-1) -> (bottom, left)
      k3 <- writeRange arr [(bottom, j) | j <- reverse [left .. right - 1]] k2
      -- left column: (bottom-1, left) -> (top+1, left)
      k4 <- writeRange arr [(i, left) | i <- reverse [top + 1 .. bottom - 1]] k3
      fillLayers arr 0 (top + 1) (left + 1) (bottom - 1) (right - 1) k4
    else
      -- Single row or single column remaining; no bottom/left cycle
      fillLayers arr 0 (top + 1) (left + 1) (bottom - 1) (right - 1) k2

-- Write consecutive numbers starting at k into the provided positions.
writeRange
  :: STUArray s (Int, Int) Int
  -> [(Int, Int)]
  -> Int
  -> ST s Int
writeRange arr positions k0 = do
  let assign (idx, val) = writeArray arr idx val
      pairs = zip positions [k0 ..]
  mapM_ assign pairs
  pure (k0 + length positions)
