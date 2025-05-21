module Queens (boardString, canAttack) where

import Data.List (intercalate)

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2)
    -- Queens cannot attack if they are at the same position
    | (r1, c1) == (r2, c2) = False
    -- Same row
    | r1 == r2 = True
    -- Same column
    | c1 == c2 = True
    -- Same diagonal
    | abs (r1 - r2) == abs (c1 - c2) = True
    -- No attack
    | otherwise = False

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
    let
        boardSize = 8
        -- Helper to get the character for a given cell (row, col)
        getCellChar :: Int -> Int -> Char
        getCellChar r c =
            case (white, black) of
                (Just (wr, wc), Just (br, bc))
                    | (r, c) == (wr, wc) && (r, c) == (br, bc) -> 'B' -- Black queen overwrites white if same position
                    | (r, c) == (wr, wc) -> 'W'
                    | (r, c) == (br, bc) -> 'B'
                    | otherwise -> '_'
                (Just (wr, wc), Nothing)
                    | (r, c) == (wr, wc) -> 'W'
                    | otherwise -> '_'
                (Nothing, Just (br, bc))
                    | (r, c) == (br, bc) -> 'B'
                    | otherwise -> '_'
                (Nothing, Nothing) -> '_'

        -- Generate a single row string
        generateRow :: Int -> String
        generateRow r =
            intercalate " " [ [getCellChar r c] | c <- [0 .. boardSize - 1] ]

        -- Generate all rows and join them with newlines
        allRows = [ generateRow r | r <- [0 .. boardSize - 1] ]
    in
        unlines allRows
