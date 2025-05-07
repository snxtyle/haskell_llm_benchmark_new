module Counting (
    Color(..),
    territories,
    territoryFor
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 
import qualified Data.Set as Set
import Data.Set (Set) 
import qualified Data.List as L -- Import Data.List for foldl'

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int) -- 1-based coordinates (row, col)

-- Helper: Parse board from [String] (0-indexed) to Map Coord Char (1-based)
parseBoard :: [String] -> Map Coord Char
parseBoard rows = Map.fromList $ do
    (r, rowStr) <- zip [0..] rows      -- r is 0-indexed row number
    (c, char)   <- zip [0..] rowStr    -- c is 0-indexed col number
    return ((r + 1, c + 1), char)      -- Coord is 1-based

-- Helper: Get 4-directionally adjacent coordinates
adjacentCoords :: Coord -> [Coord]
adjacentCoords (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

-- Helper: Core logic to find a single territory (connected empty cells) 
-- and determine its owner by checking bordering stones.
-- Assumes startCoord is an empty cell on the board.
findTerritoryAndOwnerAt :: Map Coord Char -> Int -> Int -> Coord -> (Set Coord, Maybe Color)
findTerritoryAndOwnerAt boardMap boardHeight boardWidth startCoord =
    -- BFS state: 
    --   queue: list of empty cells to visit
    --   territoryFound: Set of empty cells already identified as part of this territory (acts as 'visited' for this BFS)
    --   borderingColorsFound: Set of colors of stones found adjacent to this territory
    let bfsLoop :: [Coord] -> Set Coord -> Set Color -> (Set Coord, Set Color)
        bfsLoop [] territoryFound borderingColorsFound = (territoryFound, borderingColorsFound) -- Queue empty, BFS complete
        bfsLoop (currentCell:queueTail) territoryFound borderingColorsFound =
            -- Explore neighbors of currentCell
            let (updatedBorderingColors, newEmptyNeighborsToQueue) =
                    L.foldl' (exploreNeighbor territoryFound) -- currentCell removed from partial application
                          (borderingColorsFound, []) 
                          (adjacentCoords currentCell)
                
                -- Add newly found unique empty neighbors to the territory and the queue
                newTerritoryFound = Set.union territoryFound (Set.fromList newEmptyNeighborsToQueue)
                newQueue = queueTail ++ newEmptyNeighborsToQueue
            in bfsLoop newQueue newTerritoryFound updatedBorderingColors

        -- exploreNeighbor: processes one neighbor.
        --   currentTerritorySet: All empty cells found so far for this territory (to check if an empty neighbor is new)
        --   (accBorderingColors, accNewEmptyNeighbors): Accumulator for results
        --   neighborCoord: The specific neighbor coordinate being processed
        -- Returns: (possibly updated bordering colors, list of new empty neighbors to add to queue)
        exploreNeighbor :: Set Coord -> (Set Color, [Coord]) -> Coord -> (Set Color, [Coord])
        exploreNeighbor currentTerritorySet (accBorderingColors, accNewEmptyNeighbors) neighborCoord@(nr, nc) =
            -- Check if neighbor is within board boundaries
            if nr >= 1 && nr <= boardHeight && nc >= 1 && nc <= boardWidth then
                case Map.lookup neighborCoord boardMap of
                    Just ' ' -> -- Neighbor is an empty cell
                        if neighborCoord `Set.member` currentTerritorySet then
                            -- Already part of this territory search (e.g., visited or in queue)
                            (accBorderingColors, accNewEmptyNeighbors)
                        else
                            -- New empty cell for this territory, add to list for queueing
                            (accBorderingColors, neighborCoord : accNewEmptyNeighbors)
                    Just 'B' -> (Set.insert Black accBorderingColors, accNewEmptyNeighbors) -- Bordering black stone
                    Just 'W' -> (Set.insert White accBorderingColors, accNewEmptyNeighbors) -- Bordering white stone
                    _        -> (accBorderingColors, accNewEmptyNeighbors) -- Should not happen with valid board characters
            else
                -- Neighbor is off-board
                (accBorderingColors, accNewEmptyNeighbors)

        -- Initial BFS call: queue starts with startCoord, territory contains only startCoord, no bordering colors yet.
        (finalTerritoryCoords, finalBorderingStoneColors) = bfsLoop [startCoord] (Set.singleton startCoord) Set.empty
    
        -- Determine owner based on bordering stone colors
        owner = case Set.toList finalBorderingStoneColors of
                    [color] -> Just color -- Exactly one color borders the territory
                    _       -> Nothing    -- Zero or multiple (conflicting) colors border it
    in (finalTerritoryCoords, owner)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories boardStrings
    -- Handle empty board or board with no columns (e.g., ["", ""])
    | null boardStrings || null (head boardStrings) = []
    | otherwise =
        let boardHeight = length boardStrings
            boardWidth = length (head boardStrings)
            boardMap = parseBoard boardStrings
            
            -- Iterate through all coordinates on the board.
            -- If an unvisited empty cell is found, start a BFS (findTerritoryAndOwnerAt)
            -- to discover that territory and its owner.
            -- `iterProcess`:
            --   `visitedOverall`: Set of all coordinates already part of any found territory
            --   `foundTerritoriesAcc`: Accumulator for the (Set Coord, Maybe Color) results
            --   `coordsToCheck`: List of all (row,col) coordinates on the board, processed one by one
            iterProcess :: Set Coord -> [(Set Coord, Maybe Color)] -> [Coord] -> [(Set Coord, Maybe Color)]
            iterProcess _ foundTerritoriesAcc [] = foundTerritoriesAcc -- All coordinates checked
            iterProcess visitedOverall foundTerritoriesAcc (coord:remainingCoordsToCheck) =
                -- Check the type of cell at the current coordinate
                case Map.lookup coord boardMap of
                    Just ' ' -> -- Current coord is an empty cell
                        if coord `Set.member` visitedOverall then
                            -- Already visited as part of a previously found territory, skip
                            iterProcess visitedOverall foundTerritoriesAcc remainingCoordsToCheck
                        else
                            -- New, unvisited empty cell: this is the start of a new territory
                            let (currentTerritorySet, owner) = findTerritoryAndOwnerAt boardMap boardHeight boardWidth coord
                                newVisitedOverall = Set.union visitedOverall currentTerritorySet
                            in iterProcess newVisitedOverall ((currentTerritorySet, owner) : foundTerritoriesAcc) remainingCoordsToCheck
                    _ -> -- Not an empty cell (it's a stone, or invalid char if board spec allows)
                        iterProcess visitedOverall foundTerritoriesAcc remainingCoordsToCheck
            
            -- Generate all 1-based coordinates on the board
            allBoardCoords = [(r, c) | r <- [1..boardHeight], c <- [1..boardWidth]]
        in
        -- The order of territories in the output list is not specified as critical.
        -- Prepending to `foundTerritoriesAcc` will reverse the discovery order, which is fine.
        iterProcess Set.empty [] allBoardCoords

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor boardStrings coord@(r,c) -- Input coord is 1-based
    -- Handle empty board or board with no columns
    | null boardStrings || null (head boardStrings) = Nothing
    | otherwise =
        let boardHeight = length boardStrings
            boardWidth = length (head boardStrings)
            boardMap = parseBoard boardStrings
        in
        -- First, check if the given coord is even on the board
        if r < 1 || r > boardHeight || c < 1 || c > boardWidth then
            Nothing
        else
            -- If on board, check if the cell at coord is empty
            case Map.lookup coord boardMap of
                Just ' ' -> 
                    -- If empty, find its territory and owner
                    Just $ findTerritoryAndOwnerAt boardMap boardHeight boardWidth coord
                _        -> 
                    -- Not an empty cell (it's a stone or invalid char)
                    Nothing
