module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int) -- 1-based coordinates (row, column)

-- Helper: Get board dimensions (rows, cols)
boardDims :: [String] -> (Int, Int)
boardDims [] = (0, 0)
boardDims board = (length board, if null (head board) then 0 else length (head board))

-- Helper: Check if a 0-based coordinate is within board bounds
isValid0Based :: (Int, Int) -> (Int, Int) -> Bool
isValid0Based (r, c) (rows, cols) = r >= 0 && r < rows && c >= 0 && c < cols

-- Helper: Get character at a 0-based coordinate
getCharAt0Based :: [String] -> (Int, Int) -> Maybe Char
getCharAt0Based board (r, c) =
    let (rows, cols) = boardDims board
    in if isValid0Based (r, c) (rows, cols)
       then Just ((board !! r) !! c)
       else Nothing

-- Helper: Get 0-based neighbors for a 0-based coordinate
getNeighbors0Based :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getNeighbors0Based (r, c) (rows, cols) =
    filter (`isValid0Based` (rows, cols))
           [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

-- Helper: Convert 0-based coordinate to 1-based Coord
to1Based :: (Int, Int) -> Coord
to1Based (r, c) = (r + 1, c + 1)

-- Helper: Convert 1-based Coord to 0-based coordinate
to0Based :: Coord -> (Int, Int)
to0Based (r, c) = (r - 1, c - 1)

-- Helper: Convert char to Color
charToColor :: Char -> Maybe Color
charToColor 'B' = Just Black
charToColor 'W' = Just White
charToColor _   = Nothing

-- Core flood fill logic (BFS)
-- Returns (Set of 0-based coords in territory, Set of Colors of adjacent stones)
findTerritoryDetails :: [String] -> (Int, Int) -> (Set (Int, Int), Set Color)
findTerritoryDetails board startCoord0Based =
    bfs initialQueue initialVisited initialTerritory initialAdjacentColors
  where
    (rows, cols) = boardDims board -- Declared once in where clause
    initialQueue = [startCoord0Based]
    initialVisited = Set.singleton startCoord0Based
    initialTerritory = Set.singleton startCoord0Based
    initialAdjacentColors = Set.empty

    bfs :: [(Int, Int)] -- Queue (list acting as a queue)
        -> Set (Int, Int) -- Visited cells (empty cells only, to prevent cycles and re-processing)
        -> Set (Int, Int) -- Accumulator for territory cells (empty cells only)
        -> Set Color      -- Accumulator for adjacent stone colors
        -> (Set (Int, Int), Set Color)
    bfs [] _ territoryAcc adjacentColorsAcc = (territoryAcc, adjacentColorsAcc) -- Removed unused 'visited'
    bfs (currentCoord : queue) visited territoryAcc adjacentColorsAcc =
        let neighbors = getNeighbors0Based currentCoord (rows, cols)
            (newQueue, newVisited, newTerritoryAcc, newAdjacentColorsAcc) =
                foldl' (\(qAcc, vAcc, tAcc, aAcc) neighbor ->
                          case getCharAt0Based board neighbor of
                            Just ' ' ->
                              if not (Set.member neighbor vAcc)
                                then (neighbor : qAcc, Set.insert neighbor vAcc, Set.insert neighbor tAcc, aAcc)
                                else (qAcc, vAcc, tAcc, aAcc)
                            Just c ->
                              case charToColor c of
                                Just color -> (qAcc, vAcc, tAcc, Set.insert color aAcc)
                                Nothing -> (qAcc, vAcc, tAcc, aAcc) -- Should not happen for 'X' or invalid chars
                            Nothing -> (qAcc, vAcc, tAcc, aAcc) -- Out of bounds
                       ) (queue, visited, territoryAcc, adjacentColorsAcc) neighbors
        in bfs newQueue newVisited newTerritoryAcc newAdjacentColorsAcc

-- Main function: territories
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
    let (rows, cols) = boardDims board
        allCoords0Based = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
        allTerritories = snd $ foldl' (\(visitedEmpty, accResults) coord0Based ->
            case getCharAt0Based board coord0Based of
                Just ' ' ->
                    if not (Set.member coord0Based visitedEmpty)
                    then
                        let (currentTerritory0Based, adjacentColors) = findTerritoryDetails board coord0Based
                            owner = case Set.size adjacentColors of
                                        1 -> Just (Set.findMin adjacentColors) -- Exactly one color, so it's owned
                                        _ -> Nothing                           -- Zero or multiple colors, no owner
                            territory1Based = Set.map to1Based currentTerritory0Based
                        in (Set.union visitedEmpty currentTerritory0Based, (territory1Based, owner) : accResults)
                    else (visitedEmpty, accResults)
                _ -> (visitedEmpty, accResults) -- Not an empty cell, or already visited
        ) (Set.empty, []) allCoords0Based
    in filter (\(_, owner) -> owner /= Nothing) allTerritories -- Filter out territories with no owner

-- Main function: territoryFor
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord1Based =
    let coord0Based = to0Based coord1Based
        (rows, cols) = boardDims board
    in if not (isValid0Based coord0Based (rows, cols))
       then Nothing -- Coordinate is out of bounds
       else case getCharAt0Based board coord0Based of
            Just ' ' ->
                let (currentTerritory0Based, adjacentColors) = findTerritoryDetails board coord0Based
                    owner = case Set.size adjacentColors of
                                1 -> Just (Set.findMin adjacentColors)
                                _ -> Nothing
                    territory1Based = Set.map to1Based currentTerritory0Based
                in Just (territory1Based, owner)
            _ -> Nothing -- Coordinate is not an empty space
