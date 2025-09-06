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

-- Returns all territories on the board with their owners
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = mapMaybe (territoryFor board) allEmptyCoords
  where
    height = length board
    width = if null board then 0 else length (head board)
    allCoords = [(x, y) | y <- [1..height], x <- [1..width]]
    allEmptyCoords = filter (isEmpty board) allCoords
    -- Remove duplicates by only keeping the first occurrence of each territory
    uniqueTerritories = removeDuplicateTerritories []

removeDuplicateTerritories :: [(Set Coord, Maybe Color)] -> [(Set Coord, Maybe Color)] -> [(Set Coord, Maybe Color)]
removeDuplicateTerritories seen [] = seen
removeDuplicateTerritories seen (t:ts)
  | any (sameTerritory t) seen = removeDuplicateTerritories seen ts
  | otherwise = removeDuplicateTerritories (seen ++ [t]) ts
  where
    sameTerritory (coords1, _) (coords2, _) = coords1 == coords2

-- Returns the territory containing the specified coordinate, if it's an empty point
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board (x, y)
  | not (isEmpty board (x, y)) = Nothing
  | otherwise = Just (territoryCoords, owner)
  where
    territoryCoords = findTerritory board (x, y)
    owner = findOwner board territoryCoords

-- Find all coordinates in the territory containing the given coordinate
findTerritory :: [String] -> Coord -> Set Coord
findTerritory board start = go Set.empty [start]
  where
    go visited [] = visited
    go visited (current:rest)
      | current `Set.member` visited = go visited rest
      | isEmpty board current = 
          let neighbors = adjacentEmptyCoords board current
              newVisited = Set.insert current visited
              newQueue = rest ++ filter (not . (`Set.member` newVisited)) neighbors
          in go newVisited newQueue
      | otherwise = go visited rest

-- Find the owner of a territory (Nothing if neutral, Just color if owned)
findOwner :: [String] -> Set Coord -> Maybe Color
findOwner board territory = 
  let adjacentStones = Set.unions [adjacentStoneCoords board coord | coord <- Set.toList territory]
      blackCount = length $ filter (isStone board Black) (Set.toList adjacentStones)
      whiteCount = length $ filter (isStone board White) (Set.toList adjacentStones)
  in case (blackCount, whiteCount) of
       (0, 0) -> Nothing
       (n, 0) | n > 0 -> Just Black
       (0, n) | n > 0 -> Just White
       _ -> Nothing  -- Mixed adjacent stones means no owner

-- Get adjacent coordinates (up, down, left, right)
adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

-- Get adjacent empty coordinates
adjacentEmptyCoords :: [String] -> Coord -> [Coord]
adjacentEmptyCoords board coord = filter (isEmpty board) (adjacentCoords coord)

-- Get adjacent stone coordinates
adjacentStoneCoords :: [String] -> Coord -> Set Coord
adjacentStoneCoords board coord = Set.fromList $ filter (isStone board Black || isStone board White) (adjacentCoords coord)

-- Check if coordinate is empty
isEmpty :: [String] -> Coord -> Bool
isEmpty board (x, y) = 
  y >= 1 && y <= length board && 
  x >= 1 && x <= rowLength && 
  getBoardChar board (x, y) == ' '
  where rowLength = if null board then 0 else length (head board)

-- Check if coordinate contains a stone of given color
isStone :: [String] -> Color -> Coord -> Bool
isStone board color (x, y) =
  y >= 1 && y <= length board && 
  x >= 1 && x <= rowLength && 
  case getBoardChar board (x, y) of
    'B' -> color == Black
    'W' -> color == White
    _ -> False
  where rowLength = if null board then 0 else length (head board)

-- Get character at coordinate (1-based)
getBoardChar :: [String] -> Coord -> Char
getBoardChar board (x, y) = (board !! (y-1)) !! (x-1)
