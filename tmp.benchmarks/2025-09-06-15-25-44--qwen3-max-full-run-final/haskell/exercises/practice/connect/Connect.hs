module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

-- Convert character to Mark
charToMark :: Char -> Maybe Mark
charToMark 'X' = Just Cross
charToMark 'O' = Just Nought
charToMark _ = Nothing

-- Check if a player has won by creating a connected path
winner :: [String] -> Maybe Mark
winner board = case (hasWon Cross, hasWon Nought) of
    (True, False) -> Just Cross
    (False, True) -> Just Nought
    _ -> Nothing
  where
    height = length board
    -- Calculate width considering the staggered nature of the board
    width = maximum (map length board)
    
    -- Check if a player has won
    hasWon :: Mark -> Bool
    hasWon mark = case mark of
        Cross -> any (canReachRight (mkBoard mark)) [0..height-1]
        Nought -> any (canReachBottom (mkBoard mark)) [0..width-1]
    
    -- Create a boolean board for a specific mark
    mkBoard :: Mark -> [[Bool]]
    mkBoard mark = map (map (== Just mark) . map charToMark) board
    
    -- Check if we can reach the right edge from a position on the left edge
    canReachRight :: [[Bool]] -> Int -> Bool
    canReachRight b row
        | row < 0 || row >= height = False
        | otherwise = go (row, 0) []
      where
        go :: (Int, Int) -> [(Int, Int)] -> Bool
        go (r, c) visited
            | c >= width = True  -- Reached right edge
            | r < 0 || r >= height || c < 0 = False
            | not (safeGet r c b) = False
            | (r, c) `elem` visited = False
            | otherwise = any (\pos -> go pos ((r, c):visited)) neighbors
          where
            neighbors = [(r-1, c), (r+1, c), (r, c-1), (r, c+1), (r-1, c+1), (r+1, c-1)]
    
    -- Check if we can reach the bottom edge from a position on the top edge
    canReachBottom :: [[Bool]] -> Int -> Bool
    canReachBottom b col
        | col < 0 || col >= width = False
        | otherwise = go (0, col) []
      where
        go :: (Int, Int) -> [(Int, Int)] -> Bool
        go (r, c) visited
            | r >= height = True  -- Reached bottom edge
            | r < 0 || c < 0 || c >= width = False
            | not (safeGet r c b) = False
            | (r, c) `elem` visited = False
            | otherwise = any (\pos -> go pos ((r, c):visited)) neighbors
          where
            neighbors = [(r-1, c), (r+1, c), (r, c-1), (r, c+1), (r-1, c+1), (r+1, c-1)]
    
    -- Safe accessor for the board
    safeGet :: Int -> Int -> [[Bool]] -> Bool
    safeGet r c b
        | r < 0 || r >= length b = False
        | c < 0 || c >= length (b !! r) = False
        | otherwise = b !! r !! c
