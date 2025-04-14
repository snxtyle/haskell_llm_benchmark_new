module Counting (
    Color(..),
    territories,
    territoryFor
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set) -- Explicit import for clarity
import Data.Map.Strict (Map) -- Explicit import for clarity

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int) -- 1-based (col, row) for output API
type InternalCoord = (Int, Int) -- 0-based (row, col) for internal logic

type BoardMap = Map InternalCoord Char
type VisitedSet = Set InternalCoord -- Set of 0-based coords visited globally
type TerritoryData = (Set Coord, Maybe Color) -- Output format (Set of 1-based coords, Owner)

-- Function to parse the board string array into a map and dimensions
-- Returns the map, height, and width of the board.
parseBoard :: [String] -> (BoardMap, Int, Int)
parseBoard boardStrings =
    let height = length boardStrings
        width = if height == 0 then 0 else length (head boardStrings)
        -- Create a list of ((row, col), char) tuples for all cells
        coords = [((r, c), boardStrings !! r !! c) | r <- [0..height-1], c <- [0..width-1]]
    in (Map.fromList coords, height, width)

-- Function to get valid neighbor coordinates (up, down, left, right)
-- Ensures neighbors are within the board boundaries.
neighbors :: Int -> Int -> InternalCoord -> [InternalCoord]
neighbors height width (r, c) =
    filter (\(nr, nc) -> nr >= 0 && nr < height && nc >= 0 && nc < width)
           [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

-- Convert 0-based internal coordinate (row, col) to 1-based API coordinate (col, row)
toOneBased :: InternalCoord -> Coord
toOneBased (r, c) = (c + 1, r + 1)

-- Convert 1-based API coordinate (col, row) to 0-based internal coordinate (row, col)
toZeroBased :: Coord -> InternalCoord
toZeroBased (col, row) = (row - 1, col - 1)

-- Explore a connected empty region using Breadth-First Search (BFS)
-- starting from a given empty coordinate.
-- Returns the set of 0-based coordinates in the discovered region
-- and the set of colors of stones adjacent to this region.
exploreEmptyRegionBFS :: BoardMap -> Int -> Int -> InternalCoord -> (Set InternalCoord, Set Color)
exploreEmptyRegionBFS boardMap height width startCoord =
    -- Initial state for BFS: queue with starting cell, visited set for this region, empty set for adjacent colors
    bfs [startCoord] (Set.singleton startCoord) Set.empty
  where
    -- Recursive BFS helper function
    -- Takes the current queue, the set of visited cells in this region, and adjacent colors found so far.
    bfs [] visited adjColors = (visited, adjColors) -- Base case: Queue empty, BFS for this region is done. Return results.
    bfs (current:queue) visited adjColors =
        -- Process the current cell: explore its neighbors
        let currentNeighbors = neighbors height width current
            -- Fold over neighbors, updating visited set, adjacent colors, and finding new empty cells to add to the queue
            (newVisited, newAdjColors, newQueueAdditions) =
                foldl (processNeighbor visited adjColors) (visited, adjColors, []) currentNeighbors
        -- Continue BFS with the rest of the queue and the newly found empty cells
        in bfs (queue ++ newQueueAdditions) newVisited newAdjColors

    -- Process a single neighbor of the current cell during BFS
    processNeighbor currentVisited currentAdjColors (v, ac, qAdd) neighborCoord =
        case Map.lookup neighborCoord boardMap of
            -- Neighbor is an empty cell (' ')
            Just ' ' ->
                if Set.member neighborCoord v
                then (v, ac, qAdd) -- Already visited in this BFS traversal, do nothing
                else (Set.insert neighborCoord v, ac, qAdd ++ [neighborCoord]) -- New empty cell: mark visited for this region, add to BFS queue

            -- Neighbor is a Black stone ('B')
            Just 'B' -> (v, Set.insert Black ac, qAdd) -- Add Black to the set of adjacent colors for this region

            -- Neighbor is a White stone ('W')
            Just 'W' -> (v, Set.insert White ac, qAdd) -- Add White to the set of adjacent colors for this region

            -- Neighbor is off-board (Nothing) or an unexpected character (Just other)
            _        -> (v, ac, qAdd) -- Ignore

-- Determine the owner of a territory based on the set of adjacent stone colors.
-- If only Black stones are adjacent, owner is Black.
-- If only White stones are adjacent, owner is White.
-- Otherwise (no stones or both colors adjacent), there is no owner.
determineOwner :: Set Color -> Maybe Color
determineOwner adjColors
    | Set.size adjColors == 1 = Set.lookupMin adjColors -- Returns Just Black or Just White
    | otherwise               = Nothing                 -- 0 or 2 colors adjacent -> no owner

-- Calculate all territories on the board.
-- Returns a list of tuples, each containing the set of 1-based coordinates
-- belonging to a territory and the owner (Maybe Color) of that territory.
territories :: [String] -> [TerritoryData]
territories boardStrings
    -- Basic validation for board shape (non-empty, rectangular)
    | null boardStrings || any null boardStrings || any (\row -> length row /= length (head boardStrings)) (tail boardStrings) = []
    | otherwise =
        let (boardMap, height, width) = parseBoard boardStrings
            -- Generate all possible 0-based coordinates on the board
            allCoords = [(r, c) | r <- [0..height-1], c <- [0..width-1]]
            -- Fold over all coordinates, accumulating territories found and keeping track of globally visited cells
            -- `globalVisited` ensures we don't start a new search from within an already found territory.
        in fst $ foldl (processCoord boardMap height width) ([], Set.empty) allCoords
  where
    -- Process a single coordinate during the board scan for territories
    processCoord boardMap height width (territoryList, globalVisited) coord =
        -- Check if the coordinate has already been visited (is part of a previously found territory or is a stone)
        if Set.member coord globalVisited
        then (territoryList, globalVisited) -- Skip if already visited
        else case Map.lookup coord boardMap of
            -- Found an unvisited empty cell (' ')
            Just ' ' ->
                -- Start BFS from this cell to find the connected empty region and its adjacent stone colors
                let (regionCoords, adjColors) = exploreEmptyRegionBFS boardMap height width coord
                    -- Determine the owner of this newly found territory
                    owner = determineOwner adjColors
                    -- Convert the 0-based internal coordinates of the region to 1-based API coordinates
                    oneBasedCoords = Set.map toOneBased regionCoords
                    -- Create the territory data tuple for the result list
                    newTerritory = (oneBasedCoords, owner)
                -- Add the new territory to the result list
                -- Add all coordinates of this region to the globally visited set
                in (newTerritory : territoryList, Set.union globalVisited regionCoords)
            -- Cell is not empty (stone 'B'/'W') or off-board (shouldn't happen with allCoords)
            _ -> (territoryList, Set.insert coord globalVisited) -- Mark this non-empty cell as visited

-- Find the territory containing a specific coordinate.
-- Input `coord` is 1-based (col, row).
-- Returns `Maybe` a territory data tuple (Set of 1-based coordinates, Owner).
-- Returns `Nothing` if the coordinate is off-board, not an empty cell, or the board is invalid.
territoryFor :: [String] -> Coord -> Maybe TerritoryData
territoryFor boardStrings oneBasedCoord
    -- Basic validation for board shape
    | null boardStrings || any null boardStrings || any (\row -> length row /= length (head boardStrings)) (tail boardStrings) = Nothing
    | otherwise =
        let (boardMap, height, width) = parseBoard boardStrings
            -- Convert the 1-based input coordinate to 0-based internal coordinate
            zeroBasedCoord@(r, c) = toZeroBased oneBasedCoord
        -- Check if the coordinate is valid: within bounds and points to an empty cell (' ')
        in if r < 0 || r >= height || c < 0 || c >= width || Map.lookup zeroBasedCoord boardMap /= Just ' '
           then Nothing -- Coordinate is off-board or not an empty cell
           else
               -- Coordinate is valid and empty, explore the region starting from here
               let (regionCoords, adjColors) = exploreEmptyRegionBFS boardMap height width zeroBasedCoord
                   -- Determine the owner of the territory
                   owner = determineOwner adjColors
                   -- Convert the 0-based internal coordinates of the region to 1-based API coordinates
                   oneBasedCoords = Set.map toOneBased regionCoords
               -- Return the territory data (coordinates and owner) wrapped in Just
               in Just (oneBasedCoords, owner)
