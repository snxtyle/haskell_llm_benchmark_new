module Spiral (spiral) where

import Data.List (zipWith3)

-- | Generates a spiral matrix of size n starting with number 'start'.
generate :: Int -> Int -> [[Int]]
generate 0 _ = []          -- Base case: 0x0 matrix is empty
generate 1 start = [[start]] -- Base case: 1x1 matrix
generate n start =
    let s = n -- Use 's' for size for clarity in formulas
        -- The number of elements in the outermost layer
        layerSize = if s == 1 then 1 else 4 * s - 4
        -- The starting number for the next inner layer
        nextStart = start + layerSize
        -- Recursively generate the inner matrix
        innerMatrix = generate (n - 2) nextStart
        -- List of numbers for the current layer
        nums = [start .. nextStart - 1]

        -- Extract the four sides from the list of numbers
        top    = take s nums
        right  = take (s - 2) (drop s nums)
        bottom = reverse (take s (drop (s + s - 2) nums))
        left   = reverse (take (s - 2) (drop (s + s - 2 + s) nums))

        -- Assemble the middle rows by combining left, inner, and right parts
        middleRows = zipWith3 (\l inner r -> l : inner ++ [r]) left innerMatrix right

    in -- Combine top row, middle rows, and bottom row
       top : middleRows ++ [bottom]

-- | Returns a square matrix of the given size (Int) with elements
-- | arranged in a clockwise spiral pattern.
spiral :: Int -> [[Int]]
spiral size
  | size < 0 = error "Size cannot be negative"
  | size == 0 = []
  | otherwise = generate size 1
