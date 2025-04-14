module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | null board = Nothing
  | otherwise  =
      let rows = length board
          extracted = [ drop i (board !! i) | i <- [0..rows-1] ]  -- Drop leading spaces for each row
          getNeighbors r c = [ (r2, c2) | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, 1), (1, 1)],
                                         let r2 = r + dr,
                                         r2 >= 0 && r2 < rows,
                                         let c2 = c + dc,
                                         c2 >= 0 && c2 < length (extracted !! r2) ]
          
          hasPath startPositions endPredicate markChar =  -- BFS implementation
            let queue = [ pos | pos@(r, c) <- startPositions, extracted !! r !! c == markChar ]
                visited = queue  -- Initial visited set
                bfs [] _ = False
                bfs ((r, c):qs) vis
                  | endPredicate (r, c) = True  -- Reached end
                  | otherwise            =
                      let neighbors = filter (\pos@(r2, c2) -> extracted !! r2 !! c2 == markChar && pos `notElem` vis) (getNeighbors r c)
                          newQueue = qs ++ neighbors
                          newVisited = vis ++ neighbors
                      in bfs newQueue newVisited
            in bfs queue visited
          
          -- For Nought: Start from 'O' in row 0, end at 'O' in last row
          startsO = [(0, c) | c <- [0..length (extracted !! 0) - 1]]
          hasOWin = hasPath startsO (\(r, c) -> r == rows - 1 && extracted !! r !! c == 'O') 'O'
          
          -- For Cross: Start from 'X' in leftmost cells, end at 'X' in rightmost cells
          startsX = [(r, 0) | r <- [0..rows-1], length (extracted !! r) > 0]
          hasXWin = hasPath startsX (\(r, c) -> c == length (extracted !! r) - 1 && extracted !! r !! c == 'X') 'X'
      in if hasOWin then Just Nought else if hasXWin then Just Cross else Nothing
