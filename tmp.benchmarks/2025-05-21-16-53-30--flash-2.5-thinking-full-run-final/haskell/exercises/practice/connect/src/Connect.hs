module Connect (Mark(..), winner) where

import Data.List (filter)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList) -- Added for toList function

data Mark = Cross | Nought deriving (Eq, Show)

-- Represents a coordinate (row, effective_column)
type Coord = (Int, Int)

-- Helper to get the character at a logical coordinate (row, col)
-- The input board has 'row' leading spaces for row 'row'.
-- So, the character at logical (r, c) is at physical index (r + c) in the string for row r.
getCell :: [String] -> Int -> Int -> Maybe Char
getCell board r c =
    let numRows = length board
        numCols = if numRows == 0 then 0 else length (head board) - (numRows - 1)
    in if r >= 0 && r < numRows && c >= 0 && c < numCols
       then Just ((board !! r) !! (r + c))
       else Nothing

-- Get valid neighbors for a given coordinate (r, c)
-- These are the 6 neighbors in a pointy-top hexagonal grid, mapped to (row, effective_column)
getNeighbors :: [String] -> Coord -> [Coord]
getNeighbors board (r, c) =
    let numRows = length board
        numCols = if numRows == 0 then 0 else length (head board) - (numRows - 1)
        possibleNeighbors =
            [ (r, c - 1)     -- left
            , (r, c + 1)     -- right
            , (r - 1, c)     -- up-left
            , (r - 1, c + 1) -- up-right
            , (r + 1, c)     -- down-left
            , (r + 1, c + 1) -- down-right
            ]
    in filter (\(nr, nc) -> nr >= 0 && nr < numRows && nc >= 0 && nc < numCols) possibleNeighbors

-- Breadth-First Search (BFS) to check for connectivity
-- playerMark: the mark of the player we are checking (Cross or Nought)
-- startCoords: a list of starting coordinates for the path
-- isTarget: a predicate function to check if a coordinate is a target (winning) coordinate
bfs :: [String] -> Mark -> [Coord] -> (Coord -> Bool) -> Bool
bfs board playerMark startCoords isTarget =
    let
        charForMark Cross = 'X'
        charForMark Nought = 'O'
        targetChar = charForMark playerMark

        -- Initial queue contains all valid starting cells that have the player's mark
        initialQueue = Seq.fromList $ filter (\coord -> getCell board (fst coord) (snd coord) == Just targetChar) startCoords
        -- Initial visited set contains all cells in the initial queue
        initialVisited = Set.fromList $ toList initialQueue -- Changed Seq.toList to toList

        -- BFS loop
        loop :: Set.Set Coord -> Seq.Seq Coord -> Bool
        loop visited queue =
            case Seq.viewl queue of
                Seq.EmptyL -> False -- No more cells to visit, no path found
                coord Seq.:< restQueue ->
                    if isTarget coord
                    then True -- Path found to a target coordinate
                    else
                        let
                            -- Get unvisited neighbors that have the player's mark
                            unvisitedNeighbors =
                                filter (\n -> not (Set.member n visited) && getCell board (fst n) (snd n) == Just targetChar)
                                (getNeighbors board coord)
                            -- Add new neighbors to visited set and queue
                            newVisited = visited `Set.union` (Set.fromList unvisitedNeighbors)
                            newQueue = restQueue Seq.>< (Seq.fromList unvisitedNeighbors)
                        in
                            loop newVisited newQueue
    in
        loop initialVisited initialQueue

winner :: [String] -> Maybe Mark
winner board =
    let numRows = length board
        numCols = if numRows == 0 then 0 else length (head board) - (numRows - 1)

        -- Player O (Nought) connects top to bottom
        oStartCoords = [(0, c) | c <- [0 .. numCols - 1]]
        oIsTarget (r, _) = r == numRows - 1
        oWins = bfs board Nought oStartCoords oIsTarget

        -- Player X (Cross) connects left to right
        xStartCoords = [(r, 0) | r <- [0 .. numRows - 1]]
        xIsTarget (_, c) = c == numCols - 1
        xWins = bfs board Cross xStartCoords xIsTarget
    in
        if oWins then Just Nought
        else if xWins then Just Cross
        else Nothing
