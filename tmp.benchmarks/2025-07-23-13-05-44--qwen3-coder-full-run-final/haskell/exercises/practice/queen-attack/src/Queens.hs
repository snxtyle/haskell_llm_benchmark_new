module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines board
  where
    -- Create an empty 8x8 board filled with '_'
    emptyBoard = replicate 8 (replicate 8 '_')
    
    -- Place pieces on the board
    board = placePieces emptyBoard
    
    placePieces :: [[Char]] -> [[Char]]
    placePieces b = 
        let b1 = case white of
                   Just (r, c) -> updateBoard b r c 'W'
                   Nothing -> b
        in case black of
             Just (r, c) -> updateBoard b1 r c 'B'
             Nothing -> b1
    
    updateBoard :: [[Char]] -> Int -> Int -> Char -> [[Char]]
    updateBoard b r c piece = 
        let (beforeRows, row:afterRows) = splitAt r b
            (beforeCols, _:afterCols) = splitAt c row
        in beforeRows ++ (beforeCols ++ piece : afterCols) : afterRows

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2)
    -- Same row
    | r1 == r2 = True
    -- Same column
    | c1 == c2 = True
    -- Same diagonal (absolute difference of rows equals absolute difference of columns)
    | abs (r1 - r2) == abs (c1 - c2) = True
    -- Otherwise, no attack possible
    | otherwise = False
