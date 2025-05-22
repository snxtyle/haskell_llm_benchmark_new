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
territories board = map (\coords -> (coords, ownerOf board coords)) allTerritories
  where
    allTerritories = findAllTerritories board

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
  | not (isValidCoord board coord) = Nothing
  | not (isEmpty board coord) = Nothing
  | otherwise = Just (territory, ownerOf board territory)
  where
    territory = findTerritory board coord

-- Helper functions

-- Check if a coordinate is valid (within board bounds)
isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (x, y) = 
    y >= 1 && y <= length board && 
    x >= 1 && x <= (if null board then 0 else length (head board))

-- Check if a position is empty
isEmpty :: [String] -> Coord -> Bool
isEmpty board coord = getCell board coord == Just ' '

-- Get the character at a coordinate (1-based)
getCell :: [String] -> Coord -> Maybe Char
getCell board (x, y)
  | isValidCoord board (x, y) = Just ((board !! (y - 1)) !! (x - 1))
  | otherwise = Nothing

-- Find all territories on the board
findAllTerritories :: [String] -> [Set Coord]
findAllTerritories board = go Set.empty allCoords
  where
    height = length board
    width = if null board then 0 else length (head board)
    allCoords = [(x, y) | y <- [1..height], x <- [1..width]]
    
    go _ [] = []
    go visited (coord:rest)
      | coord `Set.member` visited = go visited rest
      | not (isEmpty board coord) = go (Set.insert coord visited) rest
      | otherwise = 
          let territory = findTerritory board coord
              newVisited = Set.union visited territory
          in territory : go newVisited rest

-- Find a territory starting from a given empty coordinate
findTerritory :: [String] -> Coord -> Set Coord
findTerritory board start = floodFill Set.empty [start]
  where
    floodFill visited [] = visited
    floodFill visited (coord:queue)
      | coord `Set.member` visited = floodFill visited queue
      | not (isValidCoord board coord) = floodFill visited queue
      | not (isEmpty board coord) = floodFill visited queue
      | otherwise = 
          let newVisited = Set.insert coord visited
              neighbors = getNeighbors coord
              newQueue = queue ++ filter (`Set.notMember` newVisited) neighbors
          in floodFill newVisited newQueue

-- Get horizontal and vertical neighbors
getNeighbors :: Coord -> [Coord]
getNeighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Determine the owner of a territory
ownerOf :: [String] -> Set Coord -> Maybe Color
ownerOf board territory = 
    let borderStones = Set.fromList $ concatMap (getBorderStones board) (Set.toList territory)
    in case Set.toList borderStones of
         [] -> Nothing
         stones | all (== 'B') stones -> Just Black
                | all (== 'W') stones -> Just White
                | otherwise -> Nothing

-- Get stones adjacent to a coordinate
getBorderStones :: [String] -> Coord -> [Char]
getBorderStones board coord = 
    mapMaybe (getCell board) (getNeighbors coord) >>= \c ->
        if c == 'B' || c == 'W' then [c] else []
