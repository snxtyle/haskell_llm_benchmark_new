module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories [] = []
territories board = 
    let allCoords = [(x, y) | y <- [1..length board], x <- [1..getBoardWidth board]]
        emptyCoords = filter (isEmpty board) allCoords
        visited = Set.empty
        results = collectTerritories board emptyCoords visited
    in results

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor [] _ = Nothing
territoryFor board coord = 
    if isEmpty board coord
    then Just (findTerritory board coord)
    else Nothing

-- Helper functions

getBoardWidth :: [String] -> Int
getBoardWidth [] = 0
getBoardWidth (row:_) = length row

isEmpty :: [String] -> Coord -> Bool
isEmpty [] _ = False
isEmpty board (x, y) = 
    y > 0 && y <= length board && 
    x > 0 && x <= getBoardWidth board && 
    (board !! (y-1)) !! (x-1) == ' '

getStoneColor :: [String] -> Coord -> Maybe Color
getStoneColor [] _ = Nothing
getStoneColor board (x, y) =
    if y <= 0 || x <= 0 || y > length board || x > getBoardWidth board
    then Nothing
    else case (board !! (y-1)) !! (x-1) of
        'B' -> Just Black
        'W' -> Just White
        _   -> Nothing

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

findTerritory :: [String] -> Coord -> (Set Coord, Maybe Color)
findTerritory board startCoord = 
    let (territory, borderColors) = exploreTerritory board (Set.singleton startCoord) (Set.singleton startCoord) Set.empty
        owner = case Set.toList borderColors of
            [] -> Nothing
            [color] -> Just color
            _ -> Nothing
    in (territory, owner)

exploreTerritory :: [String] -> Set Coord -> Set Coord -> Set Color -> (Set Coord, Set Color)
exploreTerritory board toVisit visited borderColors =
    case Set.minView toVisit of
        Nothing -> (visited, borderColors)
        Just (coord, remainingToVisit) ->
            let (newVisited, newToVisit, newBorderColors) = processCoord board coord visited remainingToVisit borderColors
            in exploreTerritory board newToVisit newVisited newBorderColors

processCoord :: [String] -> Coord -> Set Coord -> Set Coord -> Set Color -> (Set Coord, Set Coord, Set Color)
processCoord board coord visited toVisit borderColors =
    let ns = neighbors coord
        (emptyNs, stoneNs) = partitionCoords board ns
        unvisitedEmptyNs = filter (`Set.notMember` visited) emptyNs
        newVisited = Set.union visited (Set.fromList unvisitedEmptyNs)
        newToVisit = Set.union toVisit (Set.fromList unvisitedEmptyNs)
        stoneColors = Set.fromList $ mapMaybe (getStoneColor board) stoneNs
        newBorderColors = Set.union borderColors stoneColors
    in (newVisited, newToVisit, newBorderColors)

partitionCoords :: [String] -> [Coord] -> ([Coord], [Coord])
partitionCoords board coords = 
    let validCoords = filter isValidCoord coords
        (empties, nonEmpties) = partitionBy' (isEmpty board) validCoords
    in (empties, nonEmpties)
  where
    isValidCoord (x, y) = x > 0 && y > 0 && y <= length board && x <= getBoardWidth board

partitionBy' :: (a -> Bool) -> [a] -> ([a], [a])
partitionBy' pred xs = (filter pred xs, filter (not . pred) xs)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs

collectTerritories :: [String] -> [Coord] -> Set Coord -> [(Set Coord, Maybe Color)]
collectTerritories [] _ _ = []
collectTerritories _ [] _ = []
collectTerritories board (coord:coords) visited =
    if Set.member coord visited
    then collectTerritories board coords visited
    else 
        let (territory, owner) = findTerritory board coord
            newVisited = Set.union visited territory
            rest = collectTerritories board (filter (`Set.notMember` newVisited) coords) newVisited
        in (territory, owner) : rest
