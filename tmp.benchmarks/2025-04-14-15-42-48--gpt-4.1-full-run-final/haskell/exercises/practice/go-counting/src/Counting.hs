module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
-- Removed unused imports

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Helper: get the board dimensions
boardDims :: [String] -> (Int, Int)
boardDims board = (length board, if null board then 0 else length (head board))

-- Helper: get the character at a coordinate (1-based)
charAt :: [String] -> Coord -> Maybe Char
charAt board (r, c) =
    let (rows, cols) = boardDims board
    in if r >= 1 && r <= rows && c >= 1 && c <= cols
       then Just ((board !! (r - 1)) !! (c - 1))
       else Nothing

-- Helper: get all coordinates of the board
allCoords :: [String] -> [Coord]
allCoords board =
    let (rows, cols) = boardDims board
    in [(r, c) | r <- [1..rows], c <- [1..cols]]

-- Helper: get neighbors (up, down, left, right)
neighbors :: [String] -> Coord -> [Coord]
neighbors board (r, c) =
    let (rows, cols) = boardDims board
        ns = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
    in filter (\(r', c') -> r' >= 1 && r' <= rows && c' >= 1 && c' <= cols) ns

-- Helper: flood fill to find all connected empty spaces from a starting point
floodFill :: [String] -> Coord -> Set Coord
floodFill board start =
    go Set.empty [start]
  where
    go visited [] = visited
    go visited (x:xs)
        | x `Set.member` visited = go visited xs
        | charAt board x /= Just ' ' = go visited xs
        | otherwise =
            let ns = neighbors board x
                new = filter (`Set.notMember` visited) ns
            in go (Set.insert x visited) (new ++ xs)

-- Helper: determine the owner of a territory (set of empty coords)
territoryOwner :: [String] -> Set Coord -> Maybe Color
territoryOwner board coords =
    let adj = Set.fromList $ concatMap (neighbors board) (Set.toList coords)
        stones = Set.fromList $ mapMaybe (charAt board) $ Set.toList adj
    in case (Set.member 'B' stones, Set.member 'W' stones) of
        (True, False) -> Just Black
        (False, True) -> Just White
        _ -> Nothing

-- Main: find all territories on the board
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go Set.empty (allCoords board)
  where
    go _ [] = []
    go visited (c:cs)
        | c `Set.member` visited = go visited cs
        | charAt board c /= Just ' ' = go visited cs
        | otherwise =
            let terr = floodFill board c
                owner = territoryOwner board terr
                -- sort the territory coordinates for deterministic output
                terrList = Set.toAscList terr
                terrSet = Set.fromList terrList
            in (terrSet, owner) : go (Set.union visited terr) cs

-- Main: find the territory for a specific coordinate
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
    case charAt board coord of
        Just ' ' ->
            let terr = floodFill board coord
                owner = territoryOwner board terr
            in Just (terr, owner)
        _ -> Nothing
