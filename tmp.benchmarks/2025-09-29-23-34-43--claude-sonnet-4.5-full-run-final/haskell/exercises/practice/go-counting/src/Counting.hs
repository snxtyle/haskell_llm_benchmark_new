module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = 
    let allEmpty = findAllEmpty board
        visited = Set.empty
    in findAllTerritories board allEmpty visited []

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = 
    if isValidCoord board coord && isEmpty board coord
    then Just (floodFill board coord Set.empty)
    else Nothing

-- Find all empty coordinates on the board
findAllEmpty :: [String] -> Set Coord
findAllEmpty board = Set.fromList
    [ (col, row) 
    | (row, line) <- zip [1..] board
    , (col, cell) <- zip [1..] line
    , cell == ' '
    ]

-- Find all territories by processing unvisited empty spaces
findAllTerritories :: [String] -> Set Coord -> Set Coord -> [(Set Coord, Maybe Color)] -> [(Set Coord, Maybe Color)]
findAllTerritories board allEmpty visited acc =
    case Set.lookupMin (Set.difference allEmpty visited) of
        Nothing -> acc
        Just coord ->
            let (territory, owner) = floodFill board coord Set.empty
                newVisited = Set.union visited territory
            in findAllTerritories board allEmpty newVisited ((territory, owner) : acc)

-- Flood fill from a coordinate to find connected empty territory and its owner
floodFill :: [String] -> Coord -> Set Coord -> (Set Coord, Maybe Color)
floodFill board start visited =
    let territory = findTerritory board start visited
        owner = determineOwner board territory
    in (territory, owner)

-- Find all connected empty spaces starting from a coordinate
findTerritory :: [String] -> Coord -> Set Coord -> Set Coord
findTerritory board coord visited
    | Set.member coord visited = visited
    | not (isValidCoord board coord) = visited
    | not (isEmpty board coord) = visited
    | otherwise =
        let visited' = Set.insert coord visited
            neighbors = getNeighbors coord
        in foldl (\v n -> findTerritory board n v) visited' neighbors

-- Determine the owner of a territory by checking adjacent stones
determineOwner :: [String] -> Set Coord -> Maybe Color
determineOwner board territory =
    let coords = Set.toList territory
        adjacentColors = Set.fromList $ catMaybes
            [ getStoneColor board neighbor
            | coord <- coords
            , neighbor <- getNeighbors coord
            , isValidCoord board neighbor
            , not (isEmpty board neighbor)
            ]
    in case Set.toList adjacentColors of
        [color] -> Just color
        _ -> Nothing

-- Get the color of a stone at a coordinate
getStoneColor :: [String] -> Coord -> Maybe Color
getStoneColor board (col, row)
    | not (isValidCoord board (col, row)) = Nothing
    | otherwise =
        let cell = (board !! (row - 1)) !! (col - 1)
        in case cell of
            'B' -> Just Black
            'W' -> Just White
            _ -> Nothing

-- Check if a coordinate is empty
isEmpty :: [String] -> Coord -> Bool
isEmpty board (col, row)
    | not (isValidCoord board (col, row)) = False
    | otherwise = (board !! (row - 1)) !! (col - 1) == ' '

-- Check if a coordinate is valid on the board
isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (col, row) =
    row >= 1 && row <= length board &&
    col >= 1 && not (null board) && col <= length (board !! 0)

-- Get the four orthogonal neighbors of a coordinate
getNeighbors :: Coord -> [Coord]
getNeighbors (col, row) =
    [ (col, row - 1)
    , (col, row + 1)
    , (col - 1, row)
    , (col + 1, row)
    ]
