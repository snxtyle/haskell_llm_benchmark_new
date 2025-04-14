module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board = case (checkWin Nought board, checkWin Cross board) of
    (True, _) -> Just Nought
    (_, True) -> Just Cross
    _ -> Nothing

-- Check if the given player has won by connecting their sides
checkWin :: Mark -> [String] -> Bool
checkWin mark board = 
    let rows = length board
        cols = maximum (map (length . words) board)
        startPoints = getStartPoints mark rows cols
        endPoints = getEndPoints mark rows cols
        boardMap = parseBoard board
    in any (\start -> any (hasPath mark boardMap rows cols start) endPoints) startPoints

-- Get starting points for a player based on their sides
getStartPoints :: Mark -> Int -> Int -> [(Int, Int)]
getStartPoints Nought rows cols = [(0, c) | c <- [0..cols-1]]
getStartPoints Cross rows _ = [(r, 0) | r <- [0..rows-1]]

-- Get ending points for a player based on their sides
getEndPoints :: Mark -> Int -> Int -> [(Int, Int)]
getEndPoints Nought rows cols = [(rows-1, c) | c <- [0..cols-1]]
getEndPoints Cross rows _ = [(r, rows-1) | r <- [0..rows-1]]

-- Parse the board into a 2D list of Maybe Mark
parseBoard :: [String] -> [[Maybe Mark]]
parseBoard = map (map parseChar . words)
  where
    parseChar "O" = Just Nought
    parseChar "X" = Just Cross
    parseChar _ = Nothing

-- Check if there's a path from start to end for the given mark using DFS
hasPath :: Mark -> [[Maybe Mark]] -> Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
hasPath mark board rows cols start end = dfs start []
  where
    dfs (r, c) visited
        | (r, c) == end = True
        | r < 0 || r >= rows || c < 0 || c >= cols = False
        | board !! r !! c /= Just mark = False
        | (r, c) `elem` visited = False
        | otherwise = let newVisited = (r, c) : visited
                      in any (\pos -> dfs pos newVisited) (neighbors (r, c) rows cols)

-- Get valid neighboring positions considering the hexagonal grid
neighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbors (r, c) rows cols = filter inBounds
    -- In a hex grid represented this way, each position has up to 6 neighbors
    -- The exact neighbors depend on whether row is even or odd in some representations
    -- but for simplicity we'll check all possible adjacent cells and filter by bounds
    [ (r-1, c-1), (r-1, c), (r-1, c+1)
    , (r, c-1),             (r, c+1)
    , (r+1, c-1), (r+1, c), (r+1, c+1) ]
  where
    inBounds (r', c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols
