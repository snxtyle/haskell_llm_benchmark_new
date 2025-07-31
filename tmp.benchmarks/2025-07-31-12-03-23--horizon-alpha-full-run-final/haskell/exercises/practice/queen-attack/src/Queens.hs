module Queens (boardString, canAttack) where

type Pos = (Int, Int)

boardSize :: Int
boardSize = 8

boardString :: Maybe Pos -> Maybe Pos -> String
boardString mWhite mBlack =
  unlines [ unwords [ cellChar r c | c <- [0 .. boardSize - 1] ] | r <- [0 .. boardSize - 1] ]
  where
    cellChar :: Int -> Int -> String
    cellChar r c
      | Just (r, c) == mWhite = "W"
      | Just (r, c) == mBlack = "B"
      | otherwise             = "_"

canAttack :: Pos -> Pos -> Bool
canAttack (r1, c1) (r2, c2) =
  sameRow || sameCol || sameDiag
  where
    sameRow  = r1 == r2
    sameCol  = c1 == c2
    sameDiag = abs (r1 - r2) == abs (c1 - c2)
