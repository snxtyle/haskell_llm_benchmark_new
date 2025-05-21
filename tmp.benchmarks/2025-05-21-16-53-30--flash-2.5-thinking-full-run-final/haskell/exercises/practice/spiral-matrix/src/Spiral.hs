module Spiral (spiral) where

import Data.List (sortOn, groupBy)

spiral :: Int -> [[Int]]
spiral n
  | n < 0 = error "Size cannot be negative" -- Or handle as per problem specification, e.g., return []
  | n == 0 = []
  | otherwise =
      let
        -- Helper to generate (row, col, value) tuples for the spiral
        -- Parameters: top_bound, bottom_bound, left_bound, right_bound, current_value, accumulator_list
        generateCoords :: Int -> Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
        generateCoords top bottom left right currentVal acc
          | top > bottom || left > right = acc -- All layers filled, return accumulated coordinates
          | otherwise =
              let
                -- 1. Fill top row (left to right)
                topRowCoords = [(top, c, currentVal + c - left) | c <- [left .. right]]
                nextValAfterTopRow = currentVal + length topRowCoords

                -- 2. Fill right column (top+1 to bottom)
                rightColCoords = [(r, right, nextValAfterTopRow + r - (top + 1)) | r <- [top + 1 .. bottom]]
                nextValAfterRightCol = nextValAfterTopRow + length rightColCoords

                -- 3. Fill bottom row (right-1 down to left) - only if there's more than one row
                bottomRowCoords =
                  if top < bottom
                  then [(bottom, c, nextValAfterRightCol + (right - 1 - c)) | c <- [right - 1, right - 2 .. left]]
                  else []
                nextValAfterBottomRow = nextValAfterRightCol + length bottomRowCoords

                -- 4. Fill left column (bottom-1 down to top+1) - only if there's more than one column and more than two rows
                leftColCoords =
                  if left < right && top < bottom - 1
                  then [(r, left, nextValAfterBottomRow + (bottom - 1 - r)) | r <- [bottom - 1, bottom - 2 .. top + 1]]
                  else []

                -- Calculate the starting value for the next inner layer
                newCurrentVal = currentVal + length topRowCoords + length rightColCoords + length bottomRowCoords + length leftColCoords

                -- Combine current layer's coordinates and append to accumulator
                currentLayerCoords = topRowCoords ++ rightColCoords ++ bottomRowCoords ++ leftColCoords

              in
                generateCoords (top + 1) (bottom - 1) (left + 1) (right - 1) newCurrentVal (acc ++ currentLayerCoords)

        -- Generate all (row, col, value) tuples
        allCoords = generateCoords 0 (n - 1) 0 (n - 1) 1 []

        -- Convert (row, col, value) tuples to a matrix
        -- 1. Sort by row, then by column to ensure correct order for grouping
        sortedCoords = sortOn (\(r, c, _) -> (r, c)) allCoords

        -- 2. Group by row index
        groupedByRow = groupBy (\(r1, _, _) (r2, _, _) -> r1 == r2) sortedCoords

        -- 3. Extract values for each row
        matrix = map (map (\(_, _, v) -> v)) groupedByRow
      in
        matrix
