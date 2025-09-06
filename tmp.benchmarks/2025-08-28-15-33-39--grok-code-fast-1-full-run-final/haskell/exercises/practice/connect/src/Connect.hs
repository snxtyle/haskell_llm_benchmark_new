module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board = 
  let h = length board
  in if h == 0 then Nothing else
    let width = length $ drop (length $ takeWhile (==' ') (board !! 0)) (board !! 0)
        get_positions ch = [ (r, leading + k) | r <- [0..h-1], let s = board !! r, let leading = length $ takeWhile (==' ') s, let cells = drop leading s, k <- [0..length cells -1], cells !! k == ch ]
        os = get_positions 'O'
        xs = get_positions 'X'
        all_pos = os ++ xs
    in if null all_pos then Nothing else
      let is_valid (r,c) = r >= 0 && r < h && let leading = length $ takeWhile (==' ') (board !! r); cells = drop leading (board !! r) in c >= leading && c < leading + length cells
          neighbors (r,c) = filter is_valid [ (r, c-1), (r, c+1), (r-1, c + off), (r-1, c + off + 1), (r+1, c + off'), (r+1, c + off' + 1) ]
            where off = if even r then -1 else 0
                  off' = if even r then 0 else -1
          bfs starts targets neigh = go [] starts
            where go _ [] = False
                  go visited (q:qs) | q `elem` targets = True
                                    | q `elem` visited = go visited qs
                                    | otherwise = let news = filter (`notElem` visited) (neigh q)
                                                  in go (q : visited) (qs ++ news)
          o_neigh p = filter (`elem` os) (neighbors p)
          x_neigh p = filter (`elem` xs) (neighbors p)
          top_os = filter ((== 0) . fst) os
          bottom_os = filter ((== (h-1)) . fst) os
          o_wins = not (null top_os) && not (null bottom_os) && bfs top_os bottom_os o_neigh
          left_bound = [ (r, r) | r <- [0..h-1] ]
          right_bound = [ (r, r + width - 1) | r <- [0..h-1] ]
          left_xs = filter (`elem` left_bound) xs
          right_xs = filter (`elem` right_bound) xs
          x_wins = not (null left_xs) && not (null right_xs) && bfs left_xs right_xs x_neigh
      in case (o_wins, x_wins) of
           (True, False) -> Just Nought
           (False, True) -> Just Cross
           _ -> Nothing
