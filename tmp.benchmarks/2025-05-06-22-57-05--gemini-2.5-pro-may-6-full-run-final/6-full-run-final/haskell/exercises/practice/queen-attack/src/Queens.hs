module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
    -- unlines adds a newline at the end of the string, which is standard.
    unlines [buildRow r white black | r <- [0..7]]
  where
    buildRow :: Int -> Maybe (Int, Int) -> Maybe (Int, Int) -> String
    buildRow r wQueen bQueen =
        -- unwords joins the characters for cells in a row with spaces.
        unwords [getCharForCell r c wQueen bQueen | c <- [0..7]]

    getCharForCell :: Int -> Int -> Maybe (Int, Int) -> Maybe (Int, Int) -> String
    getCharForCell r c wQueen bQueen
        | Just (wr, wc) <- wQueen, wr == r, wc == c = "W"
        | Just (br, bc) <- bQueen, br == r, bc == c = "B"
        | otherwise                                = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
    r1 == r2 ||                     -- Same row
    c1 == c2 ||                     -- Same column
    abs (r1 - r2) == abs (c1 - c2)  -- Same diagonal
