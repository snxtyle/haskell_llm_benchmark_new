module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

-- | Determines the winner of a Hex game.
-- Player 'O' (Nought) wins by connecting the top to the bottom.
-- Player 'X' (Cross) wins by connecting the left to the right.
-- If no player has a winning path, it returns Nothing.
winner :: [String] -> Maybe Mark
winner board
  | hasWon Nought board = Just Nought
  | hasWon Cross board = Just Cross
  | otherwise = Nothing

-- | Checks if a specific player has won by finding a path between their two sides.
hasWon :: Mark -> [String] -> Bool
hasWon mark board =
  let height = length board
      width = if null board then 0 else length (head board)
      markChar = charForMark mark
      
      -- Get starting positions on the player's "home" side.
      starts = case mark of
                 Nought -> [(0, c) | c <- [0..width-1], getCell board (0, c) == markChar]
                 Cross  -> [(r, 0) | r <- [0..height-1], getCell board (r, 0) == markChar]

      -- Depth-first search from a starting position.
      dfs :: (Int, Int) -> [(Int, Int)] -> Bool
      dfs (r, c) visited
        -- Check if we've reached the "away" side.
        | isTarget (r, c) = True
        -- Check if we've already visited this cell.
        | (r, c) `elem` visited = False
        -- Otherwise, continue searching.
        | otherwise =
            let newVisited = (r, c) : visited
                -- Get valid neighbors (same mark, in bounds).
                neighbors = filter (\pos -> getCell board pos == markChar) (getNeighbors r c)
            -- Recurse on neighbors.
            in any (\pos -> dfs pos newVisited) neighbors
      
      -- Check if a coordinate is on the target side.
      isTarget :: (Int, Int) -> Bool
      isTarget (r, c) = case mark of
                          Nought -> r == height - 1
                          Cross  -> c == width - 1

  -- If there are no starting pieces, the player can't win.
  in if null starts then False
     -- Try searching from each starting piece.
     else any (\start -> dfs start []) starts

-- | Gets the character representation for a Mark.
charForMark :: Mark -> Char
charForMark Nought = 'O'
charForMark Cross = 'X'

-- | Safely gets a cell's character, returns ' ' if out of bounds.
getCell :: [String] -> (Int, Int) -> Char
getCell board (r, c)
  | r < 0 || r >= height || c < 0 || c >= width = ' '
  | otherwise = board !! r !! c
  where
    height = length board
    width = if null board then 0 else length (head board)

-- | Gets the 6 neighbors of a hexagonal cell in a 2D array representation.
getNeighbors :: Int -> Int -> [(Int, Int)]
getNeighbors r c = [ (r-1, c-1), (r-1, c), (r, c-1), (r, c+1), (r+1, c), (r+1, c+1) ]
