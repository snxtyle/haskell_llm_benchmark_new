module Connect (Mark(..), winner) where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe, isJust)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

type Position = (Int, Int)

winner :: [String] -> Maybe Mark
winner board = 
    let positions = parseBoard board
        oPositions = filter (\(_, _, m) -> m == 'O') positions
        xPositions = filter (\(_, _, m) -> m == 'X') positions
        oCoords = map (\(r, c, _) -> (r, c)) oPositions
        xCoords = map (\(r, c, _) -> (r, c)) xPositions
    in case (checkOWins oCoords board, checkXWins xCoords board) of
        (True, _) -> Just Nought
        (_, True) -> Just Cross
        _ -> Nothing

-- Parse the board to get all positions with their marks
parseBoard :: [String] -> [(Int, Int, Char)]
parseBoard board = concat $ zipWith parseLine [0..] board
  where
    parseLine row line = 
        let offset = countLeadingSpaces line
            content = dropWhile (== ' ') line
        in mapMaybe (parseChar row offset) (zip [0..] content)
    
    parseChar row offset (col, ch)
        | ch `elem` "OX" = Just (row, col, ch)
        | otherwise = Nothing
    
    countLeadingSpaces = length . takeWhile (== ' ')

-- Check if O wins (connects top to bottom)
checkOWins :: [Position] -> [String] -> Bool
checkOWins positions board =
    let topRow = 0
        bottomRow = length board - 1
        startPositions = filter (\(r, _) -> r == topRow) positions
        targetPositions = Set.fromList $ filter (\(r, _) -> r == bottomRow) positions
    in any (\start -> canReach start targetPositions positions) startPositions

-- Check if X wins (connects left to right)
checkXWins :: [Position] -> [String] -> Bool
checkXWins positions board =
    let leftPositions = filter (isLeftEdge board) positions
        rightPositions = Set.fromList $ filter (isRightEdge board) positions
    in any (\start -> canReach start rightPositions positions) leftPositions

-- Check if a position is on the left edge
isLeftEdge :: [String] -> Position -> Bool
isLeftEdge board (row, col) = col == 0

-- Check if a position is on the right edge  
isRightEdge :: [String] -> Position -> Bool
isRightEdge board (row, col) =
    let line = board !! row
        offset = length (takeWhile (== ' ') line)
        content = dropWhile (== ' ') line
        maxCol = length (filter (`elem` ".OX") content) - 1
    in col == maxCol

-- BFS to check if we can reach any target position from start
canReach :: Position -> Set.Set Position -> [Position] -> Bool
canReach start targets allPositions = 
    let posSet = Set.fromList allPositions
    in bfs [start] Set.empty
  where
    bfs [] _ = False
    bfs (current:queue) visited
        | current `Set.member` targets = True
        | current `Set.member` visited = bfs queue visited
        | otherwise = 
            let newVisited = Set.insert current visited
                neighbors = getNeighbors current posSet
                newQueue = queue ++ filter (`Set.notMember` newVisited) neighbors
            in bfs newQueue newVisited

-- Get hexagonal neighbors of a position
getNeighbors :: Position -> Set.Set Position -> [Position]
getNeighbors (row, col) validPositions =
    let candidates = [ (row - 1, col - 1), (row - 1, col)
                     , (row, col - 1), (row, col + 1)
                     , (row + 1, col), (row + 1, col + 1) ]
    in filter (`Set.member` validPositions) candidates
