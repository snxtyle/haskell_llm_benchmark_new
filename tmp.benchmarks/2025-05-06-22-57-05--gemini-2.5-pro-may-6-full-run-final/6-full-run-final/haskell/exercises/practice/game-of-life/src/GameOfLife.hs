module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid
    -- Handle empty grid or grid with empty rows (e.g., [], [[]])
    | null grid || null (head grid) = []
    | otherwise =
        let numRows = length grid
            numCols = length (head grid) -- Safe due to the guard above

            -- Safely gets the state of a cell at (rQuery, cQuery).
            -- Out-of-bounds cells are considered dead (0).
            -- Captures 'grid', 'numRows', 'numCols' from the outer scope.
            getCellStateAt rQuery cQuery =
                if rQuery < 0 || rQuery >= numRows || cQuery < 0 || cQuery >= numCols
                then 0 -- Out of bounds is dead
                else (grid !! rQuery) !! cQuery -- Access element from the original grid

            -- Counts live neighbors for a cell at (rCell, cCell).
            -- Captures 'getCellStateAt' from the outer scope.
            countLiveNeighborsFor rCell cCell =
                sum [ getCellStateAt (rCell + dr) (cCell + dc)
                    | dr <- [-1, 0, 1]
                    , dc <- [-1, 0, 1]
                    , not (dr == 0 && dc == 0) -- Exclude the cell itself
                    ]

            -- Determines the next state of a cell based on its current state and live neighbor count.
            determineNextCellState :: Int -> Int -> Int
            determineNextCellState 1 numLiveNeighbors -- Current cell is alive
                | numLiveNeighbors == 2 || numLiveNeighbors == 3 = 1 -- Lives on
                | otherwise                                      = 0 -- Dies (underpopulation or overpopulation)
            determineNextCellState 0 numLiveNeighbors -- Current cell is dead
                | numLiveNeighbors == 3                          = 1 -- Becomes alive (reproduction)
                | otherwise                                      = 0 -- Stays dead
            determineNextCellState invalidState _ =
                error ("Invalid cell state: " ++ show invalidState ++ ". Expected 0 or 1.")

        -- Construct the new grid by applying the rules to each cell.
        -- The state of each cell in the new generation is calculated based on
        -- the state of the 'grid' in the current generation.
        in [ [ let currentCellState = (grid !! r) !! c -- Current state from original grid (r,c are in bounds)
                   liveNeighborsCount = countLiveNeighborsFor r c
               in determineNextCellState currentCellState liveNeighborsCount
             | c <- [0 .. numCols - 1] ]
           | r <- [0 .. numRows - 1] ]
