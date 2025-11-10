module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set, empty, singleton, insert, member, union, deleteFindMin, map)
import Data.Foldable (foldl', toList)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int) -- 1-based coordinate
type InternalCoord = (Int, Int) -- 0-based coordinate

-- | Returns a list of all territories on the board.
-- Each territory is a pair of a set of coordinates and its owner.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board
    | null board = []
    | otherwise = map formatTerritory foundTerritories
  where
    rows = length board
    cols = length (head board)
    allCoords = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]

    -- Main processing loop using a strict fold to iterate through all coordinates
    (foundTerritories, _) = foldl' processCoord ([], empty) allCoords

    -- Process a single coordinate, finding a new territory if one starts here
    processCoord (terrList, visited) coord@(r, c)
        | member coord visited = (terrList, visited)
        | board !! r !! c == ' ' =
            let (newTerritory, newAdjacentColors) = exploreTerritory board coord
                owner = determineOwner newAdjacentColors
                visited' = union visited newTerritory
            in ((newTerritory, owner) : terrList, visited')
        | otherwise = (terrList, visited) -- Not an empty point

    -- Convert internal 0-based coords to 1-based Coords for the final output
    formatTerritory (coords, owner) = (Data.Set.map to1Based coords, owner)

-- | Finds the territory containing a specific coordinate.
-- Returns Nothing if the coordinate is not on an empty point.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | null board = Nothing
    | not (isValidCoord (rows, cols) internalCoord) = Nothing
    | board !! r !! c /= ' ' = Nothing
    | otherwise =
        let (territoryCoords, adjColors) = exploreTerritory board internalCoord
            owner = determineOwner adjColors
        in Just (Data.Set.map to1Based territoryCoords, owner)
  where
    rows = length board
    cols = length (head board)
    internalCoord@(r, c) = from1Based coord

-- Helper function to convert from 1-based to 0-based coordinates
from1Based :: Coord -> InternalCoord
from1Based (r, c) = (r - 1, c - 1)

-- Helper function to convert from 0-based to 1-based coordinates
to1Based :: InternalCoord -> Coord
to1Based (r, c) = (r + 1, c + 1)

-- Checks if a 0-based coordinate is within the board dimensions
isValidCoord :: (Int, Int) -> InternalCoord -> Bool
isValidCoord (rows, cols) (r, c) = r >= 0 && r < rows && c >= 0 && c < cols

-- Gets the valid 4-directional neighbors of a coordinate
getNeighbors :: (Int, Int) -> InternalCoord -> [InternalCoord]
getNeighbors (rows, cols) (r, c) = filter (isValidCoord (rows, cols)) potentialNeighbors
  where
    potentialNeighbors = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

-- Explores a territory starting from a given empty coordinate using a flood-fill (BFS-like) algorithm.
-- Returns the set of coordinates in the territory and the set of adjacent stone colors.
exploreTerritory :: [String] -> InternalCoord -> (Set InternalCoord, Set Color)
exploreTerritory board startCoord = go (singleton startCoord) empty empty
  where
    rows = length board
    cols = length (head board)

    go queue territory adjColors
      | null queue = (territory, adjColors)
      | otherwise =
          let (coord, queue') = deleteFindMin queue
          in if member coord territory -- Avoid cycles within the current search
             then go queue' territory adjColors
             else
               let (r, c) = coord
                   point = board !! r !! c
               in if point /= ' '
                  then error "exploreTerritory called on non-empty point" -- Should not happen with current logic
                  else
                    let neighbors = getNeighbors (rows, cols) coord
                        (newQueue, newAdjColors) = foldl' processNeighbor (queue', adjColors) neighbors
                        territory' = insert coord territory
                    in go newQueue territory' newAdjColors

    processNeighbor (q, adjSet) nCoord@(nr, nc) =
      let point = board !! nr !! nc
      in case point of
           ' ' -> (insert nCoord q, adjSet)
           'B' -> (q, insert Black adjSet)
           'W' -> (q, insert White adjSet)
           _   -> (q, adjSet) -- Should not happen with valid input

-- Determines the owner of a territory based on the set of adjacent colors.
determineOwner :: Set Color -> Maybe Color
determineOwner adjColors = case toList adjColors of
    []      -> Nothing -- No adjacent stones
    [c]     -> Just c  -- Adjacent to only one color
    _       -> Nothing -- Adjacent to both colors (neutral)
