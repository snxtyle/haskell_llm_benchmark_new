module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | anyPath 'O' = Just Nought
  | anyPath 'X' = Just Cross
  | otherwise   = Nothing
  where
    h = length board
    w = if h == 0 then 0 else length (head board)

    -- Check if a coordinate is within the board bounds
    inBounds (r,c) = r >= 0 && r < h && c >= 0 && c < w

    -- The six neighbours on a hex grid
    neighbors (r,c) =
      filter inBounds
        [ (r-1, c)
        , (r-1, c+1)
        , (r,   c-1)
        , (r,   c+1)
        , (r+1, c-1)
        , (r+1, c)
        ]

    -- Does mark m have a connecting path?
    anyPath :: Char -> Bool
    anyPath m =
      let
        -- Starting edge cells for this mark
        starts
          | m == 'O'  = [ (0, c) | c <- [0..w-1], board !! 0 !! c == m ]
          | otherwise = [ (r, 0) | r <- [0..h-1], board !! r !! 0 == m ]

        -- Target condition: bottom edge for 'O', right edge for 'X'
        target (r,c)
          | m == 'O'  = r == h - 1
          | otherwise = c == w - 1

        -- DFS stack search, carrying visited coords
        go [] _ = False
        go ((r,c):stk) vis
          | (r,c) `elem` vis    = go stk vis
          | board !! r !! c /= m = go stk ((r,c):vis)
          | target (r,c)        = True
          | otherwise           = go (neighbors (r,c) ++ stk) ((r,c):vis)
      in
        go starts []
