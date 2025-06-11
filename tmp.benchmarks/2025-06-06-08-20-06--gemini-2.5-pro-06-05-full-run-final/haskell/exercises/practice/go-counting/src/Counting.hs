module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

type Board = Map Coord Char

-- | Parses a list of strings into a map-based board representation and its dimensions.
parseBoard :: [String] -> (Board, (Int, Int))
parseBoard rows =
    let board = Map.fromList
            [ ((x, y), char)
            | (y, row) <- zip [1..] rows
            , (x, char) <- zip [1..] row
            ]
        dims = if null rows then (0, 0) else (length (head rows), length rows)
    in (board, dims)

-- | Returns the four direct neighbors of a coordinate.
neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- | Determines the owner of a territory based on its bordering stone colors.
determineOwner :: Set Color -> Maybe Color
determineOwner borders
    | Set.size borders == 1 = Set.lookupMin borders
    | otherwise = Nothing

-- | Explores a territory starting from a given empty coordinate using BFS.
-- Returns the set of coordinates in the territory and the set of bordering colors.
exploreFrom :: Coord -> Board -> (Set Coord, Set Color)
exploreFrom start board = go (Set.singleton start) [start] Set.empty
  where
    go :: Set Coord -> [Coord] -> Set Color -> (Set Coord, Set Color)
    go territory [] borders = (territory, borders)
    go territory (c:queue) borders =
        let (newTerritory, newQueue, newBorders) =
                foldl' (processNeighbor board) (territory, [], Set.empty) (neighbors c)
        in go newTerritory (queue ++ newQueue) (Set.union borders newBorders)

-- | Processes a single neighbor during the BFS traversal.
processNeighbor :: Board -> (Set Coord, [Coord], Set Color) -> Coord -> (Set Coord, [Coord], Set Color)
processNeighbor board (terr, q, brdrs) n =
    case Map.lookup n board of
        Just ' ' ->
            if Set.member n terr
            then (terr, q, brdrs)
            else (Set.insert n terr, n : q, brdrs)
        Just 'B' -> (terr, q, Set.insert Black brdrs)
        Just 'W' -> (terr, q, Set.insert White brdrs)
        _        -> (terr, q, brdrs)

-- | Finds all territories on the board.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories boardStr =
    let (board, (width, height)) = parseBoard boardStr
        allCoords = [(x, y) | x <- [1..width], y <- [1..height]]
    in fst $ foldl' (discover board) ([], Set.empty) allCoords
  where
    discover board (found, visited) coord =
        if Set.member coord visited || Map.lookup coord board /= Just ' '
        then (found, visited)
        else
            let (territory, borders) = exploreFrom coord board
                owner = determineOwner borders
                newVisited = Set.union visited territory
            in ((territory, owner) : found, newVisited)

-- | Finds the territory containing a specific coordinate.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor boardStr coord =
    let (board, _) = parseBoard boardStr
    in case Map.lookup coord board of
        Just ' ' ->
            let (territory, borders) = exploreFrom coord board
                owner = determineOwner borders
            in Just (territory, owner)
        _ -> Nothing
