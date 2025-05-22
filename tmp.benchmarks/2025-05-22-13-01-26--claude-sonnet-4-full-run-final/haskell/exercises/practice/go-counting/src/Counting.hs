module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = 
    let allEmpty = getAllEmptyCoords board
        visited = Set.empty
    in findAllTerritories board allEmpty visited

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | not (isValidCoord board coord) = Nothing
    | not (isEmpty board coord) = Nothing
    | otherwise = 
        let territory = floodFill board coord Set.empty
            owner = getTerritoryOwner board territory
        in Just (territory, owner)

-- Helper functions

getAllEmptyCoords :: [String] -> Set Coord
getAllEmptyCoords board = 
    Set.fromList [(x, y) | (y, row) <- zip [1..] board,
                          (x, cell) <- zip [1..] row,
                          cell == ' ']

findAllTerritories :: [String] -> Set Coord -> Set Coord -> [(Set Coord, Maybe Color)]
findAllTerritories board remaining visited
    | Set.null remaining = []
    | otherwise = 
        let coord = Set.findMin remaining
            territory = floodFill board coord Set.empty
            owner = getTerritoryOwner board territory
            newVisited = Set.union visited territory
            newRemaining = Set.difference remaining territory
        in (territory, owner) : findAllTerritories board newRemaining newVisited

floodFill :: [String] -> Coord -> Set Coord -> Set Coord
floodFill board coord visited
    | Set.member coord visited = visited
    | not (isValidCoord board coord) = visited
    | not (isEmpty board coord) = visited
    | otherwise = 
        let newVisited = Set.insert coord visited
            neighbors = getNeighbors coord
        in foldl (\acc neighbor -> floodFill board neighbor acc) newVisited neighbors

getNeighbors :: Coord -> [Coord]
getNeighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (x, y) = 
    y >= 1 && y <= length board && 
    x >= 1 && x <= length (head board)

isEmpty :: [String] -> Coord -> Bool
isEmpty board (x, y) = 
    isValidCoord board (x, y) && 
    (board !! (y - 1)) !! (x - 1) == ' '

getStone :: [String] -> Coord -> Maybe Color
getStone board (x, y)
    | not (isValidCoord board (x, y)) = Nothing
    | otherwise = 
        case (board !! (y - 1)) !! (x - 1) of
            'B' -> Just Black
            'W' -> Just White
            _ -> Nothing

getTerritoryOwner :: [String] -> Set Coord -> Maybe Color
getTerritoryOwner board territory = 
    let territoryList = Set.toList territory
        allNeighbors = concatMap getNeighbors territoryList
        adjacentStones = mapMaybe (getStone board) allNeighbors
        uniqueColors = Set.fromList adjacentStones
    in case Set.toList uniqueColors of
        [color] -> Just color
        _ -> Nothing
