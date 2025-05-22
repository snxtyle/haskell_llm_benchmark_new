module Connect (Mark(..), winner) where

import Data.List (elemIndex)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

type Position = (Int, Int)

winner :: [String] -> Maybe Mark
winner board = 
    case (hasNoughtWon parsedBoard, hasCrossWon parsedBoard) of
        (True, _) -> Just Nought
        (_, True) -> Just Cross
        _ -> Nothing
  where
    parsedBoard = parseBoard board

-- Parse the board into a list of (position, mark) tuples
parseBoard :: [String] -> [(Position, Mark)]
parseBoard rows = catMaybes $ concat 
    [ [ parseCell row col (rows !! row !! col) 
      | col <- [0..length (rows !! row) - 1] 
      ]
    | row <- [0..length rows - 1]
    ]
  where
    parseCell row col 'O' = Just ((row, col), Nought)
    parseCell row col 'X' = Just ((row, col), Cross)
    parseCell _ _ _ = Nothing

-- Get all positions for a specific mark
getPositions :: [(Position, Mark)] -> Mark -> [Position]
getPositions board mark = [pos | (pos, m) <- board, m == mark]

-- Check if Nought (O) has won by connecting top to bottom
hasNoughtWon :: [(Position, Mark)] -> Bool
hasNoughtWon board = 
    let noughtPositions = Set.fromList $ getPositions board Nought
        topPositions = filter (\(row, _) -> row == 0) $ Set.toList noughtPositions
        bottomRow = maximum $ map (fst . fst) board
    in any (\start -> canReachBottom start noughtPositions bottomRow) topPositions

-- Check if Cross (X) has won by connecting left to right
hasCrossWon :: [(Position, Mark)] -> Bool
hasCrossWon board =
    let crossPositions = Set.fromList $ getPositions board Cross
        leftPositions = filter (isLeftmost board) $ Set.toList crossPositions
        rightmostCol = getRightmostCol board
    in any (\start -> canReachRight start crossPositions rightmostCol) leftPositions

-- Check if a position is on the leftmost edge considering the hex board shape
isLeftmost :: [(Position, Mark)] -> Position -> Bool
isLeftmost board (row, col) =
    let rowPositions = [c | ((r, c), _) <- board, r == row]
    in null rowPositions || col == minimum rowPositions

-- Get the rightmost column for each row
getRightmostCol :: [(Position, Mark)] -> [(Int, Int)]
getRightmostCol board =
    [(row, maximum [c | ((r, c), _) <- board, r == row]) | row <- [0..maximum (map (fst . fst) board)]]

-- BFS to check if we can reach the bottom row from a starting position
canReachBottom :: Position -> Set.Set Position -> Int -> Bool
canReachBottom start positions targetRow = bfs [start] Set.empty
  where
    bfs [] _ = False
    bfs (pos@(row, col):rest) visited
        | row == targetRow = True
        | Set.member pos visited = bfs rest visited
        | otherwise = 
            let neighbors = getNeighbors pos positions
                newVisited = Set.insert pos visited
                unvisited = filter (`Set.notMember` newVisited) neighbors
            in bfs (rest ++ unvisited) newVisited

-- BFS to check if we can reach the rightmost column from a starting position
canReachRight :: Position -> Set.Set Position -> [(Int, Int)] -> Bool
canReachRight start positions rightmostCols = bfs [start] Set.empty
  where
    bfs [] _ = False
    bfs (pos@(row, col):rest) visited
        | isRightmost pos = True
        | Set.member pos visited = bfs rest visited
        | otherwise = 
            let neighbors = getNeighbors pos positions
                newVisited = Set.insert pos visited
                unvisited = filter (`Set.notMember` newVisited) neighbors
            in bfs (rest ++ unvisited) newVisited
    
    isRightmost (row, col) = 
        case lookup row rightmostCols of
            Just rightCol -> col == rightCol
            Nothing -> False

-- Get valid neighbors for a hex position
getNeighbors :: Position -> Set.Set Position -> [Position]
getNeighbors (row, col) positions =
    filter (`Set.member` positions) 
        [ (row - 1, col - 1), (row - 1, col)     -- top-left, top-right
        , (row, col - 1), (row, col + 1)         -- left, right
        , (row + 1, col), (row + 1, col + 1)     -- bottom-left, bottom-right
        ]
