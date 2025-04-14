module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, listToMaybe)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Convert board to a 2D list of Maybe Color for easier processing
parseBoard :: [String] -> [[Maybe Color]]
parseBoard board = map (map parseChar) board
  where
    parseChar 'B' = Just Black
    parseChar 'W' = Just White
    parseChar _ = Nothing

-- Get board dimensions
boardDimensions :: [[a]] -> (Int, Int)
boardDimensions board = (length board, length (head board))

-- Get all empty coordinates on the board
getEmptyCoords :: [[Maybe Color]] -> [Coord]
getEmptyCoords board = [(r, c) | r <- [1..rows], c <- [1..cols], isEmpty r c]
  where
    (rows, cols) = boardDimensions board
    isEmpty r c = Nothing == (board !! (r-1) !! (c-1))

-- Get adjacent coordinates (horizontal and vertical only)
getAdjacent :: Int -> Int -> Coord -> [Coord]
getAdjacent rows cols (r, c) = filter validCoord [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  where
    validCoord (r', c') = r' >= 1 && r' <= rows && c' >= 1 && c' <= cols

-- Flood fill to find a territory starting from a coordinate
findTerritory :: [[Maybe Color]] -> Set Coord -> Coord -> Set Coord
findTerritory board visited start = floodFill Set.empty [start]
  where
    (rows, cols) = boardDimensions board
    floodFill territory [] = territory
    floodFill territory (coord@(r, c):queue)
      | Set.member coord visited || Set.member coord territory || isStone coord = floodFill territory queue
      | otherwise = floodFill (Set.insert coord territory) (queue ++ getAdjacent rows cols coord)
    isStone (r, c) = isJust (board !! (r-1) !! (c-1))
    isJust (Just _) = True
    isJust Nothing = False

-- Get the owner of a territory by checking adjacent stones
getOwner :: [[Maybe Color]] -> Set Coord -> Maybe Color
getOwner board territory = case Set.toList colors of
    [color] -> color
    _ -> Nothing
  where
    (rows, cols) = boardDimensions board
    adjacentStones = concatMap (getAdjacentStones board rows cols) (Set.toList territory)
    colors = Set.fromList $ catMaybes adjacentStones

getAdjacentStones :: [[Maybe Color]] -> Int -> Int -> Coord -> [Maybe Color]
getAdjacentStones board rows cols coord = map getStone (getAdjacent rows cols coord)
  where
    getStone (r, c) = board !! (r-1) !! (c-1)

-- Find all territories on the board
territories :: [String] -> [(Set Coord, Maybe Color)]
territories boardStr = findTerritories (parseBoard boardStr) Set.empty (getEmptyCoords parsedBoard)
  where
    parsedBoard = parseBoard boardStr
    findTerritories _ visited [] = []
    findTerritories board visited (coord:coords)
      | Set.member coord visited = findTerritories board visited coords
      | otherwise = 
          let territory = findTerritory board visited coord
              owner = getOwner board territory
              newVisited = Set.union visited territory
          in (territory, owner) : findTerritories board newVisited coords

-- Find the territory containing a specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor boardStr coord = listToMaybe $ filter (containsCoord coord) (territories boardStr)
  where
    containsCoord c (territory, _) = Set.member c territory
