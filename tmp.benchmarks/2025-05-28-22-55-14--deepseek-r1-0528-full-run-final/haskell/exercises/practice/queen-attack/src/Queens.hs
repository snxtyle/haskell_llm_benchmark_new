module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = 
  let header = "  a b c d e f g h"
      rowStr i = 
        let displayRow = 8 - i
            squares = [[sq (i,j)] | j <- [0..7]]  -- Wrap each Char in String
        in show displayRow ++ " " ++ unwords squares ++ " " ++ show displayRow
      sq pos | white == Just pos = 'W'
             | black == Just pos = 'B'
             | otherwise = '_'
  in header ++ "\n" ++ unlines [ rowStr i | i <- [0..7] ] ++ header

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) 
    | r1 == r2 = True
    | c1 == c2 = True
    | abs (r1 - r2) == abs (c1 - c2) = True
    | otherwise = False
