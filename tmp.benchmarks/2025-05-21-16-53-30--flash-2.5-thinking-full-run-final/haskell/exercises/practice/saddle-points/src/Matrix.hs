module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.List (maximum, minimum)

-- | Finds saddle points in a 2D array (matrix).
-- A saddle point is an element which is the maximum in its row
-- and the minimum in its column.
--
-- The input 'matrix' is expected to have (Int, Int) as its index type
-- and Int as its element type, representing tree heights in a grid.
saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =
    let
        -- Get the bounds of the matrix: ((minRow, minCol), (maxRow, maxCol))
        ((rMin, cMin), (rMax, cMax)) = bounds matrix

        -- Generate all possible (row, column) indices in the matrix
        allIndices = [(r, c) | r <- [rMin .. rMax], c <- [cMin .. cMax]]

        -- Helper function to get all values in a specific row
        getRowValues :: Int -> [Int]
        getRowValues r = [matrix ! (r, c) | c <- [cMin .. cMax]]

        -- Helper function to get all values in a specific column
        getColValues :: Int -> [Int]
        getColValues c = [matrix ! (r, c) | r <- [rMin .. rMax]]

        -- Check if the value at (r, c) is the maximum in its row
        isMaxInRow :: Int -> Int -> Bool
        isMaxInRow r c =
            let val = matrix ! (r, c)
                rowVals = getRowValues r
            in
                -- Ensure the row is not empty before calling maximum
                -- (though for a non-empty matrix, rows won't be empty)
                not (null rowVals) && val == maximum rowVals

        -- Check if the value at (r, c) is the minimum in its column
        isMinInCol :: Int -> Int -> Bool
        isMinInCol r c =
            let val = matrix ! (r, c)
                colVals = getColValues c
            in
                -- Ensure the column is not empty before calling minimum
                not (null colVals) && val == minimum colVals

    in
        -- Filter all indices to find those that are both max in row and min in column
        filter (\(r, c) -> isMaxInRow r c && isMinInCol r c) allIndices
