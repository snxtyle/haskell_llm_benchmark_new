module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (sort)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Get the stone color at a coordinate, or Nothing if empty or out of bounds
stoneAt :: [String] -> Coord -> Maybe Color
stoneAt board (r,c) =
    if r < 1 || r > length board || c < 1 || c > length (head board)
    then Nothing
    else case (board !! (r-1)) !! (c-1) of
        'B' -> Just Black
        'W' -> Just White
        _   -> Nothing

-- Check if a coordinate is empty (space)
isEmpty :: [String] -> Coord -> Bool
isEmpty board (r,c) =
    if r < 1 || r > length board || c < 1 || c > length (head board)
    then False
    else (board !! (r-1)) !! (c-1) == ' '

-- Get neighbors (up, down, left, right)
neighbors :: Coord -> [Coord]
neighbors (r,c) = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

-- Flood fill to find all connected empty coords starting from coord
floodFillEmpty :: [String] -> Coord -> Set Coord
floodFillEmpty board start = go Set.empty [start]
  where
    go visited [] = visited
    go visited (x:xs)
      | Set.member x visited = go visited xs
      | not (isEmpty board x) = go visited xs
      | otherwise = go (Set.insert x visited) (neighbors x ++ xs)

-- Determine the owner of a territory given the board and the territory coords
ownerOf :: [String] -> Set Coord -> Maybe Color
ownerOf board territory =
    let adjColors = Set.fromList $ mapMaybe (stoneAt board) $ concatMap neighbors (Set.toList territory)
    in case Set.toList adjColors of
        [c] -> Just c
        _   -> Nothing

-- territoryFor returns the territory containing the coordinate and its owner
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | not (isEmpty board coord) = Nothing
    | otherwise =
        let territory = floodFillEmpty board coord
            owner = ownerOf board territory
        in Just (territory, owner)

-- territories returns all territories on the board with their owners
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go Set.empty allCoords []
  where
    rows = length board
    cols = if null board then 0 else length (head board)
    allCoords = [(r,c) | r <- [1..rows], c <- [1..cols]]

    go _ [] acc = acc
    go visited (x:xs) acc
      | Set.member x visited = go visited xs acc
      | not (isEmpty board x) = go visited xs acc
      | otherwise =
          let territory = floodFillEmpty board x
              owner = ownerOf board territory
              visited' = Set.union visited territory
          in go visited' xs ((territory, owner):acc)
