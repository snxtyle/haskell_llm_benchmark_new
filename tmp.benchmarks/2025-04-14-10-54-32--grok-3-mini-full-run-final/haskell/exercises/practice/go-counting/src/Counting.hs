module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, catMaybes, listToMaybe)
import Data.List (nub)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)  -- (row, column), 1-based

-- Helper: Check if a coordinate is within the board bounds
isInBounds :: [String] -> Coord -> Bool
isInBounds board (r, c) = r >= 1 && r <= height && c >= 1 && c <= width
  where
    height = length board
    width = if height > 0 then length (head board) else 0

-- Helper: Get the character at a coordinate
getCharAt :: [String] -> Coord -> Maybe Char
getCharAt board coord@(r, c)
  | isInBounds board coord = Just ((board !! (r-1)) !! (c-1))  -- 1-based to 0-based
  | otherwise = Nothing

-- Helper: Check if a position is empty (space character)
isEmpty :: [String] -> Coord -> Bool
isEmpty board coord = case getCharAt board coord of
  Just ' ' -> True
  _        -> False

-- Helper: Get the 4-directional neighbors of a coordinate
neighbors :: Coord -> [Coord]
neighbors (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

-- Helper: Determine the color of a stone at a coordinate
stoneColor :: [String] -> Coord -> Maybe Color
stoneColor board coord = case getCharAt board coord of
  Just 'B' -> Just Black
  Just 'b' -> Just Black
  Just 'W' -> Just White
  Just 'w' -> Just White
  _        -> Nothing  -- Not a stone

-- Helper: BFS to find connected components of empty spaces
bfs :: [String] -> Set.Set Coord -> Coord -> Set.Set Coord
bfs board visited start
  | not (isEmpty board start) = Set.empty
  | Set.member start visited = Set.empty
  | otherwise = go [start] (Set.singleton start)
  where
    go queue visitedSet = case queue of
      [] -> visitedSet
      current:rest ->
        let ns = filter (\n -> isInBounds board n && isEmpty board n && not (Set.member n visitedSet)) (neighbors current)
            newVisited = foldl (flip Set.insert) visitedSet ns
            newQueue = rest ++ ns
        in go newQueue newVisited

-- territories: Find all territories and their owners
territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories board =
  let emptyCoords = [(r, c) | r <- [1..height], c <- [1..width], isEmpty board (r, c)]
      height = length board
      width = if height > 0 then length (head board) else 0
      allComponents = nub [bfs board Set.empty start | start <- emptyCoords]  -- Start BFS from each empty coord with empty visited set
      uniqueComponents = [comp | comp <- allComponents, not (Set.null comp)]  -- Remove empty sets
  in map (determineOwner board) uniqueComponents  -- For each component, determine the owner
  where
    determineOwner board component =
      let adjacentStones = catMaybes [stoneColor board n | coord <- Set.toList component, n <- neighbors coord, isInBounds board n, not (isEmpty board n)]
          uniqueColors = nub adjacentStones
      in (component, case uniqueColors of
               [Just Black] -> Just Black  -- All adjacent stones are Black
               [Just White] -> Just White  -- All adjacent stones are White
               _            -> Nothing)   -- Mixed or no stones

-- territoryFor: Find the territory containing the given coordinate
territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord
  | not (isEmpty board coord) = Nothing  -- Not empty, so no territory
  | otherwise =
      let component = bfs board Set.empty coord  -- Get the connected component
          territoriesList = territories board  -- Get all territories
          matchingTerritory = listToMaybe [(set, owner) | (set, owner) <- territoriesList, Set.member coord set]
      in case matchingTerritory of
           Just t -> Just t  -- t is now the tuple (set, owner)
           Nothing -> Nothing  -- Safeguard
