module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set, empty, union, member, insert, notMember)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Helper function to get valid neighbors
getNeighbors :: [String] -> Coord -> [Coord]
getNeighbors board (x, y) = 
    filter (\(i, j) -> i >= 0 && j >= 0 && i < length board && j < length (if null board then 0 else head board))
    [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Helper function to get the character at a coordinate
getCharAt :: [String] -> Coord -> Char
getCharAt board (x, y) = (board !! x) !! y

-- Find territory using BFS starting from an empty cell
findTerritory :: [String] -> Coord -> Set Coord -> (Set Coord, Set Char)
findTerritory board start visited = bfs [start] empty empty visited
  where
    bfs [] territory borders _ = (territory, borders)
    bfs (current:queue) territory borders visited =
        if member current visited
        then bfs queue territory borders visited
        else
            let newVisited = insert current visited
                charAtCurrent = getCharAt board current
            in if charAtCurrent /= ' '
               then bfs queue territory (insert charAtCurrent borders) newVisited
               else
                   let neighbors = getNeighbors board current
                       newQueue = filter (`notMember` newVisited) neighbors ++ queue
                   in bfs newQueue (insert current territory) borders newVisited

-- Convert 0-based coordinates to 1-based
to1Based :: Set Coord -> Set Coord
to1Based = Set.map (\(x, y) -> (x + 1, y + 1))

-- Find all territories on the board
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = 
    let rows = length board
        cols = if null board then 0 else length (head board)
        allCoords = [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
        emptyCoords = filter (\(i, j) -> getCharAt board (i, j) == ' ') allCoords
        (result, _) = findTerritories board emptyCoords empty []
    in result
  where
    findTerritories _ [] visited result = (result, visited)
    findTerritories board (coord:coords) visited result =
        if member coord visited || getCharAt board coord /= ' '
        then findTerritories board coords visited result
        else
            let (territory, borders) = findTerritory board coord visited
                owner = determineOwner borders
                newVisited = visited `union` territory
                territory1Based = to1Based territory
            in findTerritories board coords newVisited ((territory1Based, owner) : result)

-- Determine owner based on border stones
determineOwner :: Set Char -> Maybe Color
determineOwner borders
    | Set.null borders = Nothing
    | Set.member 'B' borders && not (Set.member 'W' borders) = Just Black
    | Set.member 'W' borders && not (Set.member 'B' borders) = Just White
    | otherwise = Nothing

-- Find territory for a specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board (x, y) =
    -- Convert 1-based input to 0-based for internal processing
    let x0 = x - 1
        y0 = y - 1
        rows = length board
        cols = if null board then 0 else length (head board)
    in if x0 < 0 || x0 >= rows || y0 < 0 || y0 >= cols || getCharAt board (x0, y0) /= ' '
       then Nothing
       else
           let allTerritories = territories board
               found = filter (\(terr, _) -> member (x, y) terr) allTerritories
           in case found of
                [] -> Nothing
                (terr:_) -> Just terr
