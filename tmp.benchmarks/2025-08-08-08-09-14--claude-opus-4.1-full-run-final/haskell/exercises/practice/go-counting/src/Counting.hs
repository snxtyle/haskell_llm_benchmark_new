module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (mapMaybe)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = nub $ map snd $ mapMaybe (territoryForInternal board) allCoords
  where
    height = length board
    width = if height > 0 then length (head board) else 0
    allCoords = [(x, y) | y <- [1..height], x <- [1..width]]

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = 
    case territoryForInternal board coord of
        Just (_, territory) -> Just territory
        Nothing -> Nothing

-- Internal helper that returns both the coordinate and its territory
territoryForInternal :: [String] -> Coord -> Maybe (Coord, (Set Coord, Maybe Color))
territoryForInternal board coord@(x, y)
    | not (isValidCoord board coord) = Nothing
    | getCell board coord /= Just ' ' = Nothing
    | otherwise = Just (coord, findTerritory board coord)

-- Find the territory starting from a given empty coordinate
findTerritory :: [String] -> Coord -> (Set Coord, Maybe Color)
findTerritory board startCoord = (territory, owner)
  where
    territory = floodFill board startCoord Set.empty
    owner = determineOwner board territory

-- Flood fill to find all connected empty spaces
floodFill :: [String] -> Coord -> Set Coord -> Set Coord
floodFill board coord visited
    | not (isValidCoord board coord) = visited
    | Set.member coord visited = visited
    | getCell board coord /= Just ' ' = visited
    | otherwise = foldl (\acc neighbor -> floodFill board neighbor acc) 
                       (Set.insert coord visited) 
                       (neighbors coord)

-- Get the four orthogonal neighbors
neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Determine the owner of a territory
determineOwner :: [String] -> Set Coord -> Maybe Color
determineOwner board territory = 
    let borderStones = nub $ concatMap (getBorderStones board) (Set.toList territory)
    in case borderStones of
        [] -> Nothing
        stones | all (== 'B') stones -> Just Black
               | all (== 'W') stones -> Just White
               | otherwise -> Nothing

-- Get the stones (non-empty cells) adjacent to a coordinate
getBorderStones :: [String] -> Coord -> [Char]
getBorderStones board coord = 
    mapMaybe (\neighbor -> 
        case getCell board neighbor of
            Just c | c /= ' ' -> Just c
            _ -> Nothing
    ) (neighbors coord)

-- Check if a coordinate is valid for the board
isValidCoord :: [String] -> Coord -> Bool
isValidCoord board (x, y) = 
    y >= 1 && y <= length board && 
    x >= 1 && x <= (if null board then 0 else length (head board))

-- Get the cell at a coordinate (1-indexed)
getCell :: [String] -> Coord -> Maybe Char
getCell board (x, y)
    | not (isValidCoord board (x, y)) = Nothing
    | otherwise = Just $ (board !! (y - 1)) !! (x - 1)
