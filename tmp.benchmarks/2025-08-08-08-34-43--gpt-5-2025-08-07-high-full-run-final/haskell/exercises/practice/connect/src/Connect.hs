module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board =
  let grid = parse board
  in if wins Cross grid
       then Just Cross
       else if wins Nought grid
         then Just Nought
         else Nothing
  where
    -- Remove spaces to get the logical board cells per row
    parse :: [String] -> [String]
    parse = map (filter (/= ' '))

    wins :: Mark -> [String] -> Bool
    wins mark g =
      let h = length g
          token = case mark of
                    Cross  -> 'X'
                    Nought -> 'O'

          rowLen :: Int -> Int
          rowLen r = length (g !! r)

          valid :: (Int, Int) -> Bool
          valid (r, c) = r >= 0 && r < h && c >= 0 && c < rowLen r

          neighbors :: (Int, Int) -> [(Int, Int)]
          neighbors (r, c) =
            filter valid
              [ (r - 1, c)
              , (r - 1, c + 1)
              , (r,     c - 1)
              , (r,     c + 1)
              , (r + 1, c - 1)
              , (r + 1, c)
              ]

          at :: (Int, Int) -> Char
          at (r, c) = (g !! r) !! c

          isGoal :: (Int, Int) -> Bool
          isGoal (r, c) =
            case mark of
              Cross  -> c == rowLen r - 1
              Nought -> r == h - 1

          starts :: [(Int, Int)]
          starts =
            case mark of
              Cross ->
                [ (r, 0)
                | r <- [0 .. h - 1]
                , rowLen r > 0
                , at (r, 0) == token
                ]
              Nought ->
                if h == 0
                  then []
                  else
                    [ (0, c)
                    | c <- [0 .. rowLen 0 - 1]
                    , at (0, c) == token
                    ]

          search :: [(Int, Int)] -> [(Int, Int)] -> Bool
          search visited [] = False
          search visited (p:ps)
            | p `elem` visited = search visited ps
            | isGoal p = True
            | otherwise =
                let visited' = p : visited
                    next =
                      [ q
                      | q <- neighbors p
                      , at q == token
                      , q `notElem` visited'
                      ]
                in search visited' (next ++ ps)
      in search [] starts
