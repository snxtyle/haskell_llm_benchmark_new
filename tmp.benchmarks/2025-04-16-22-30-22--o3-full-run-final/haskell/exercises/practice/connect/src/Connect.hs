module Connect (Mark(..), winner) where

-- We removed the dependency on Data.Set to avoid requiring the
-- “containers” package, replacing it with a simple list‑based DFS.

data Mark = Cross | Nought deriving (Eq, Show)

-- | Determine the winner on a Hex board described as a list of strings.
--   Each string may contain spaces for indentation; every non‑space
--   character must be '.', 'O', or 'X'.
--
--   * Player  O  tries to connect the **top** side with the **bottom**.
--   * Player  X  tries to connect the **left**  side with the **right**.
winner :: [String] -> Maybe Mark
winner rawRows
  | null board = Nothing
  | hasPath 'O' topEdge  bottomReached = Just Nought
  | hasPath 'X' leftEdge rightReached  = Just Cross
  | otherwise                          = Nothing
  where
    -- Strip out all spaces – they are only used for pretty printing.
    board :: [[Char]]
    board = map (filter (/= ' ')) rawRows

    height :: Int
    height = length board

    -- Return the width of a given row (they can vary).
    rowWidth :: Int -> Int
    rowWidth r = length (board !! r)

    --------------------------------------------------------------------------
    -- Helpers dealing with board coordinates
    --------------------------------------------------------------------------
    inside :: (Int, Int) -> Bool
    inside (r,c) =
      r >= 0 && r < height &&
      c >= 0 && c < rowWidth r

    neighbours :: (Int, Int) -> [(Int, Int)]
    neighbours (r,c) = filter inside
      [ (r-1, c    )   -- north‑west
      , (r-1, c+1  )   -- north‑east
      , (r  , c-1  )   -- west
      , (r  , c+1  )   -- east
      , (r+1, c-1  )   -- south‑west
      , (r+1, c    )   -- south‑east
      ]

    cellAt :: (Int, Int) -> Char
    cellAt (r,c) = (board !! r) !! c

    --------------------------------------------------------------------------
    -- Generic path search for a given player mark
    --------------------------------------------------------------------------
    hasPath :: Char                 -- ^ The mark we are looking for ('O' or 'X')
            -> [(Int,Int)]          -- ^ Starting positions
            -> ((Int,Int) -> Bool)  -- ^ Goal predicate
            -> Bool
    hasPath ch starts goal = any (dfs []) starts
      where
        dfs :: [(Int,Int)] -> (Int,Int) -> Bool
        dfs seen pos
          | not (inside pos)      = False
          | cellAt pos /= ch      = False
          | goal pos              = True
          | pos `elem` seen       = False
          | otherwise             =
              let seen' = pos : seen
                  next  = filter (\p -> cellAt p == ch && p `notElem` seen') (neighbours pos)
              in any (dfs seen') next

    --------------------------------------------------------------------------
    -- Edge and goal predicates for both players
    --------------------------------------------------------------------------
    topEdge, leftEdge :: [(Int,Int)]
    topEdge  = [ (0, c) | c <- [0 .. rowWidth 0 - 1], cellAt (0,c) == 'O' ]
    leftEdge = [ (r, 0) | r <- [0 .. height   - 1],  rowWidth r > 0, cellAt (r,0) == 'X' ]

    bottomReached, rightReached :: (Int,Int) -> Bool
    bottomReached (r, _) = r == height - 1
    rightReached  (r, c) = c == rowWidth r - 1
