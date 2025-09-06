module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  let rows = [buildRow i white black | i <- [0..7]]
  in concat rows ++ "\n"
  where
    buildRow i w b =
      let chars = [cellChar i j w b | j <- [0..7]]
          rowContent = unwords (map (:[]) chars)
      in rowContent ++ "\n"
    cellChar i j whitePos blackPos =
      case (whitePos, blackPos) of
        (Just (rw, cw), _) | (i, j) == (rw, cw) -> 'W'
        (_, Just (rb, cb)) | (i, j) == (rb, cb) -> 'B'
        _ -> '_'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
  r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)
