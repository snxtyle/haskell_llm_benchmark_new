module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner boardWithSpaces
  | null board || null (head board) = Nothing
  | checkWin Cross board = Just Cross
  | checkWin Nought board = Just Nought
  | otherwise = Nothing
  where
    board = map (filter (/= ' ')) boardWithSpaces

checkWin :: Mark -> [String] -> Bool
checkWin player board = any isTarget reachable
  where
    (h, w) = (length board, length (head board))

    (starts, isTarget) = case player of
      Cross  -> ([(r, 0) | r <- [0..h - 1]], \(_, c) -> c == w - 1)
      Nought -> ([(0, c) | c <- [0..w - 1]], \(r, _) -> r == h - 1)

    playerChar = case player of
      Cross  -> 'X'
      Nought -> 'O'

    initialQueue = filter (\(r, c) -> (board !! r !! c) == playerChar) starts
    reachable = dfs initialQueue []

    dfs :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    dfs [] visited = visited
    dfs (current:queue) visited
      | current `elem` visited = dfs queue visited
      | otherwise =
          let (r, c) = current
              
              potentialNeighbors =
                [ (r, c - 1), (r, c + 1)
                , (r - 1, c), (r - 1, c + 1)
                , (r + 1, c - 1), (r + 1, c)
                ]

              isValid (nr, nc) = nr >= 0 && nr < h && nc >= 0 && nc < w
              isPlayerPiece (nr, nc) = (board !! nr !! nc) == playerChar

              neighbors = filter (\pos -> isValid pos && isPlayerPiece pos) potentialNeighbors
              
              newQueue = neighbors ++ queue
              newVisited = current : visited
          in
              dfs newQueue newVisited
