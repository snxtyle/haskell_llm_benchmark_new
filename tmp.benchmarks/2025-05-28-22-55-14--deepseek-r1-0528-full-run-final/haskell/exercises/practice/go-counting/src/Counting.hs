module Counting (
    Color(..),
    territories,
    territoryFor
) where

import qualified Data.Set as S
import Data.List (foldl')

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

to1Based :: Coord -> Coord
to1Based (r, c) = (r+1, c+1)

inBounds :: [String] -> Coord -> Bool
inBounds board (r, c) = 
    let rows = length board
        cols = if null board then 0 else length (head board)
    in r >= 0 && r < rows && c >=0 && c < cols

getChar :: [String] -> Coord -> Char
getChar board (r, c) = (board !! r) !! c

neighbors :: Coord -> [Coord]
neighbors (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

bfs :: [String] -> Coord -> (S.Set Coord, S.Set Color)
bfs board start = 
    if not (inBounds board start) || getChar board start /= ' '
    then (S.empty, S.empty)
    else loop [start] S.empty S.empty
    where
        loop [] visited colors = (visited, colors)
        loop (p:ps) visited colors
            | S.member p visited = loop ps visited colors
            | otherwise = 
                let newVisited = S.insert p visited
                    ns = neighbors p
                    validNs = filter (inBounds board) ns
                    (newPs, newColors) = foldl' (f newVisited) (ps, colors) validNs
                in loop newPs newVisited newColors
        f visited (psAcc, colorsAcc) nCoord
            | S.member nCoord visited = (psAcc, colorsAcc)
            | otherwise = 
                case getChar board nCoord of
                    ' ' -> (nCoord:psAcc, colorsAcc)
                    'B' -> (psAcc, S.insert Black colorsAcc)
                    'W' -> (psAcc, S.insert White colorsAcc)
                    _   -> (psAcc, colorsAcc)

territories :: [String] -> [(S.Set Coord, Maybe Color)]
territories board = 
    let rows = length board
        cols = if null board then 0 else length (head board)
        allCoords = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
        (_, territoriesList) = foldl' f (S.empty, []) allCoords
        f (visited, acc) coord
            | S.member coord visited = (visited, acc)
            | getChar board coord == ' ' = 
                let (region, colors) = bfs board coord
                    owner = case S.toList colors of
                             [Black] -> Just Black
                             [White] -> Just White
                             _       -> Nothing
                    region1 = S.map to1Based region
                in (visited `S.union` region, (region1, owner) : acc)
            | otherwise = (visited, acc)
    in reverse territoriesList

territoryFor :: [String] -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor board (x,y) = 
    let r0 = x-1
        c0 = y-1
        coord0 = (r0, c0)
    in if not (inBounds board coord0) || getChar board coord0 /= ' '
       then Nothing
       else 
           let (region, colors) = bfs board coord0
               owner = case S.toList colors of
                        [Black] -> Just Black
                        [White] -> Just White
                        _       -> Nothing
               region1 = S.map to1Based region
           in Just (region1, owner)
