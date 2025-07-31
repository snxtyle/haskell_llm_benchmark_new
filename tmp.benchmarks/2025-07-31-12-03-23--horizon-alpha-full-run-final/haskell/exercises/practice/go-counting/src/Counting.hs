module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Internal representation of a cell on the board
data Cell = Empty | BlackStone | WhiteStone deriving (Eq)

-- Convert character to Cell
charToCell :: Char -> Cell
charToCell 'B' = BlackStone
charToCell 'W' = WhiteStone
charToCell _   = Empty

-- Dimensions
height :: [String] -> Int
height = length

width :: [String] -> Int
width [] = 0
width (r:_) = length r

-- Check coordinate bounds (1-based)
inBounds :: [String] -> Coord -> Bool
inBounds board (x, y) =
  x >= 1 && y >= 1 && x <= width board && y <= height board

-- Read cell at coordinate (1-based)
cellAt :: [String] -> Coord -> Maybe Cell
cellAt board (x, y)
  | not (inBounds board (x, y)) = Nothing
  | otherwise =
      let row = board !! (y - 1)
      in if x <= length row then Just (charToCell (row !! (x - 1))) else Nothing

-- Convert Cell to maybe Color for stones
cellColor :: Cell -> Maybe Color
cellColor BlackStone = Just Black
cellColor WhiteStone = Just White
cellColor Empty = Nothing

-- 4-neighbors (up, down, left, right)
neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- Explore a territory (connected empty region) starting from a given empty coord.
-- Returns the set of coords in the region and the set of adjacent stone colors.
floodTerritory :: [String] -> Coord -> Set Coord -> (Set Coord, Set Color)
floodTerritory board start visited0 =
  go [start] visited0 Set.empty
  where
    go [] visited colors = (visited, colors)
    go (c:cs) visited colors
      | not (inBounds board c) = go cs visited colors
      | Set.member c visited   = go cs visited colors
      | otherwise =
          let visited' = Set.insert c visited
          in case cellAt board c of
               Just Empty ->
                 let next = [ n | n <- neighbors c, inBounds board n ]
                 in go (next ++ cs) visited' colors
               Just stone ->
                 case cellColor stone of
                   Just col -> go cs visited' (Set.insert col colors)
                   Nothing  -> go cs visited' colors
               Nothing -> go cs visited' colors

-- Build all territories by scanning the board, exploring connected empty regions once.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
  let h = height board
      w = width board
      allCoords = [ (x,y) | y <- [1..h], x <- [1..w] ]
      step (seen, acc) c =
        case cellAt board c of
          Just Empty ->
            if Set.member c seen
              then (seen, acc)
              else
                let (seen', colors) = floodTerritory board c seen
                    region = Set.difference seen' seen
                    owner = case Set.toList colors of
                              [Black] -> Just Black
                              [White] -> Just White
                              _       -> Nothing
                in (seen', (region, owner) : acc)
          _ -> (seen, acc)
      (_, result) = foldl' step (Set.empty, []) allCoords
  in reverse result

-- Determine the territory containing a specific coordinate.
-- Returns Nothing if the coordinate is out of bounds or not empty.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  case cellAt board coord of
    Just Empty ->
      let (seen', colors) = floodTerritory board coord Set.empty
          region = seen'
          owner = case Set.toList colors of
                    [Black] -> Just Black
                    [White] -> Just White
                    _       -> Nothing
      in Just (region, owner)
    _ -> Nothing
