module Connect (Mark(..), winner) where

import Data.Set (Set)
import qualified Data.Set as Set
-- Using Prelude's list functions like head, length, null, map, filter, (!!)
-- foldl is also from Prelude.

data Mark = Cross | Nought deriving (Eq, Show)

-- Parses the input string representation of the board into a 2D list of Chars.
-- Each string in the input list represents a row.
-- Characters 'X', 'O', '.' are game cells. Spaces are filtered out.
-- e.g., ". O . X ." becomes ['.', 'O', '.', 'X', '.']
-- This handles the visual parallelogram layout by focusing on cell characters.
parseBoard :: [String] -> [[Char]]
parseBoard = map (filter (/= ' '))

-- Defines the 6 neighbors of a cell (r, c) in the skewed grid representation
-- commonly used for Hex. On a parallelogram grid:
-- (r-1, c)   (r-1, c+1)
--   \   /
-- (r, c-1)---(r,c)---(r, c+1)
--   /   \
-- (r+1, c-1) (r+1, c)
getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (r, c) =
    [ (r, c - 1), (r, c + 1),   -- Horizontal (left, right)
      (r - 1, c), (r - 1, c + 1), -- Upper cells (relative to (r,c) in the skewed grid)
      (r + 1, c - 1), (r + 1, c)  -- Lower cells (relative to (r,c) in the skewed grid)
    ]

-- Depth First Search (DFS) to find a path of a player's stones.
dfs :: (Int, Int)       -- Current cell (row, col) being visited
    -> Char             -- Player's mark ('X' or 'O')
    -> ((Int, Int) -> Bool) -- Target condition function (e.g., is cell on the target edge?)
    -> Set (Int, Int)   -- Set of already visited cells for the current search
    -> [[Char]]         -- The game board (assumed to be rectangular after parsing)
    -> Int              -- Total number of rows in the board
    -> Int              -- Total number of columns in the board
    -> (Bool, Set (Int, Int)) -- Returns a tuple: (True if a path to target is found, updated set of visited cells)
dfs (r, c) playerMark isTarget visited board numRows numCols =
    -- Check if the cell is out of bounds
    if r < 0 || r >= numRows || c < 0 || c >= numCols then (False, visited)
    -- Check if the cell contains the player's mark and has not been visited
    else if (board !! r !! c) /= playerMark || Set.member (r, c) visited then (False, visited)
    else
        -- Mark the current cell as visited
        let newVisited = Set.insert (r, c) visited
        in
        -- Check if the current cell satisfies the target condition (is on the target edge)
        if isTarget (r, c) then (True, newVisited)
        else
            -- Recursively explore unvisited neighbors
            let neighbors = getNeighbors (r, c)
            in
            -- Use foldl to iterate through neighbors, searching for a path.
            -- The 'visited' set is threaded through recursive calls.
            -- If a path is found via any neighbor ('foundAcc' becomes True), subsequent neighbors are not explored for this path.
            foldl (\(foundAcc, visitedAcc) neighborCoord ->
                if foundAcc then (True, visitedAcc) -- Path already found
                else dfs neighborCoord playerMark isTarget visitedAcc board numRows numCols
            ) (False, newVisited) neighbors

-- Checks if a specific player has a winning connection on the board.
checkWin :: Char             -- Player's mark ('X' or 'O')
         -> [(Int, Int)]     -- List of starting cells for this player (e.g., first column for 'X', first row for 'O')
         -> ((Int, Int) -> Bool) -- Target condition for this player (e.g., reach last column for 'X', last row for 'O')
         -> [[Char]]         -- The game board
         -> Int              -- Number of rows
         -> Int              -- Number of columns
         -> Bool
checkWin playerMark startCells isTarget board numRows numCols =
    -- Perform DFS starting from each of the player's potential start cells.
    -- The 'visited' set is accumulated across these DFS calls for efficiency (avoids re-exploring).
    -- 'fst' extracts the boolean result (True if a win was found).
    fst $ foldl (\(found, visitedAcc) startCell ->
        if found then (True, visitedAcc) -- Win already found from a previous startCell, no need to search further
        else
            -- If not already found, perform DFS from the current startCell.
            -- Pass the accumulated 'visitedAcc' to the DFS.
            let (pathFound, newVisitedAcc) = dfs startCell playerMark isTarget visitedAcc board numRows numCols
            in (pathFound, newVisitedAcc)
    ) (False, Set.empty) startCells

-- Determines the winner of the Hex game from the given board state.
winner :: [String] -> Maybe Mark
winner boardStrings
    -- Handle empty board or board with rows of no cells (e.g. from input like ["", ""]).
    | null processedBoard || null (head processedBoard) = Nothing
    | otherwise =
        if xWins then Just Cross
        else if oWins then Just Nought
        else Nothing
  where
    processedBoard = parseBoard boardStrings
    numRows = length processedBoard
    -- Assuming a parallelogram board, all rows in processedBoard will have the same length.
    -- This is crucial for (board !! r !! c) accesses and defining numCols.
    numCols = length (head processedBoard)

    -- Check for Player X win: connect left edge (column 0) to right edge (column numCols-1).
    -- Start cells are all 'X's in the first column.
    startXCells = [(r, 0) | r <- [0..numRows-1], (processedBoard !! r !! 0) == 'X']
    -- Target for X is to reach any cell in the last column.
    isTargetX (_, c) = c == numCols - 1
    xWins = checkWin 'X' startXCells isTargetX processedBoard numRows numCols

    -- Check for Player O win: connect top edge (row 0) to bottom edge (row numRows-1).
    -- Start cells are all 'O's in the first row.
    startYCells = [(0, c) | c <- [0..numCols-1], (processedBoard !! 0 !! c) == 'O']
    -- Target for O is to reach any cell in the last row.
    isTargetO (r, _) = r == numRows - 1
    oWins = checkWin 'O' startYCells isTargetO processedBoard numRows numCols
