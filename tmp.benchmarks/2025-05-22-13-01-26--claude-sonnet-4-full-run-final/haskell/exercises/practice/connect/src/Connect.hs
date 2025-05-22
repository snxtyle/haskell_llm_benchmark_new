module Connect (Mark(..), winner) where

import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | null board = Nothing
  | hasWinningPath Cross board = Just Cross
  | hasWinningPath Nought board = Just Nought
  | otherwise = Nothing

-- Check if a player has a winning path
hasWinningPath :: Mark -> [String] -> Bool
hasWinningPath mark board = 
  let coords = parseBoard board
      playerPositions = Set.fromList [pos | (pos, m) <- coords, m == mark]
  in case mark of
       Cross -> hasLeftRightConnection playerPositions board
       Nought -> hasTopBottomConnection playerPositions board

-- Parse board into coordinates and marks
parseBoard :: [String] -> [((Int, Int), Mark)]
parseBoard board = 
  concat [parseRow r row | (r, row) <- zip [0..] board]
  where
    parseRow r row = [((r, c), mark) | (c, char) <- zip [0..] row, 
                                       char /= ' ' && char /= '.', 
                                       let mark = charToMark char]

charToMark :: Char -> Mark
charToMark 'X' = Cross
charToMark 'O' = Nought
charToMark _ = error "Invalid mark"

-- Check if Cross (X) has left-right connection
hasLeftRightConnection :: Set.Set (Int, Int) -> [String] -> Bool
hasLeftRightConnection positions board =
  let leftEdge = Set.filter (\(r, c) -> c == 0) positions
      rightEdge = Set.filter (\(r, c) -> c == length (board !! r) - 1) positions
  in any (\start -> any (canReach positions start) rightEdge) leftEdge

-- Check if Nought (O) has top-bottom connection  
hasTopBottomConnection :: Set.Set (Int, Int) -> [String] -> Bool
hasTopBottomConnection positions board =
  let topEdge = Set.filter (\(r, c) -> r == 0) positions
      bottomEdge = Set.filter (\(r, c) -> r == length board - 1) positions
  in any (\start -> any (canReach positions start) bottomEdge) topEdge

-- Check if two positions are connected through valid moves
canReach :: Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
canReach positions start end = 
  dfs positions (Set.singleton start) end
  where
    dfs :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> (Int, Int) -> Bool
    dfs allPos visited target
      | target `Set.member` visited = True
      | otherwise = 
          let newPositions = Set.fromList [pos | pos <- Set.toList allPos,
                                                 pos `Set.notMember` visited,
                                                 any (areAdjacent pos) (Set.toList visited)]
          in if Set.null newPositions
             then False
             else dfs allPos (Set.union visited newPositions) target

-- Check if two positions are adjacent in hex grid
areAdjacent :: (Int, Int) -> (Int, Int) -> Bool
areAdjacent (r1, c1) (r2, c2) =
  let dr = r2 - r1
      dc = c2 - c1
  in (dr, dc) `elem` [(0, 1), (0, -1), (1, 0), (-1, 0), (1, -1), (-1, 1)]
