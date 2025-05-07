module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (find)

data Color = Black | White
    deriving (Eq, Ord, Show)

type Coord = (Int, Int)

-- look up a board cell: Just Black/White for stones, Nothing for empty or OOB
valueAt :: [String] -> Coord -> Maybe Color
valueAt board (r,c)
    | r < 1 || r > height || c < 1 || c > width = Nothing
    | otherwise = case board !! (r-1) !! (c-1) of
        'B' -> Just Black
        'W' -> Just White
        _   -> Nothing
  where
    height = length board
    width  = if null board then 0 else length (head board)

-- the four orthogonal neighbors, filtered to in-bounds
neighbours :: [String] -> Coord -> [Coord]
neighbours board (r,c) =
    let h = length board
        w = if null board then 0 else length (head board)
        inBounds (r',c') = r' >= 1 && r' <= h && c' >= 1 && c' <= w
        cand = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]
    in filter inBounds cand

-- flood-fill from a starting empty coord
explore :: [String] -> Set Coord -> [Coord] -> Set Coord
explore board visited [] = visited
explore board visited (x:xs)
    | x `Set.member` visited        = explore board visited xs
    | valueAt board x /= Nothing    = explore board visited xs
    | otherwise =
        let visited' = Set.insert x visited
            xs'      = neighbours board x ++ xs
        in explore board visited' xs'

-- for any empty coord, get its full connected component
componentAt :: [String] -> Coord -> Set Coord
componentAt board coord = explore board Set.empty [coord]

-- find all empty components on the board
allComponents :: [String] -> [Set Coord]
allComponents board =
    let height = length board
        width  = if null board then 0 else length (head board)
        allCoords   = [(r,c) | r <- [1..height], c <- [1..width]]
        emptyCoords = filter (\coord -> valueAt board coord == Nothing) allCoords

        go _      []     = []
        go seen (x:xs)
            | x `Set.member` seen = go seen xs
            | otherwise =
                let comp     = componentAt board x
                    seen'    = Set.union seen comp
                in comp : go seen' xs
    in go Set.empty emptyCoords

-- determine the owner (if any) of an empty component:
-- look at all adjacent stones; if they are all the same color, that's the owner
ownerOf :: [String] -> Set Coord -> Maybe Color
ownerOf board comp =
    let adjColors = Set.fromList $
          mapMaybe (valueAt board) $
          concatMap (neighbours board) (Set.toList comp)
    in case Set.toList adjColors of
         [col] -> Just col
         _     -> Nothing

-- PUBLIC API

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
    -- only keep those with a definite owner
    let comps = allComponents board
    in [ (comp, Just col)
       | comp <- comps
       , Just col <- [ownerOf board comp]
       ]

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
    case valueAt board coord of
      Just _  -> Nothing      -- not an empty point
      Nothing ->
        let comp = componentAt board coord
            own  = ownerOf board comp
        in Just (comp, own)
