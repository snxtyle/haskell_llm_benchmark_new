module WordSearch (search, CharPos(..), WordPos(..)) where

-- A position in the grid
data CharPos = CharPos { col :: Int  -- ^ Column number (1-based)
                       , row :: Int  -- ^ Row number    (1-based)
                       } deriving (Eq, Show)

-- The position of a whole word (start and end, inclusive)
data WordPos = WordPos { start :: CharPos
                       , end   :: CharPos
                       } deriving (Eq, Show)

-- | Given a rectangular grid of upper-case letters (all rows must be the same
-- length) and a list of words, return, for every word, either the coordinates
-- of its first and last letter or Nothing if the word cannot be found.
--
--   search ["ABC",
--           "FDE",
--           "GH I"] ["AB", "FED"] ==
--   [("AB",  Just (WordPos (CharPos 1 1) (CharPos 2 1))),
--    ("FED", Just (WordPos (CharPos 1 2) (CharPos 3 2)))]
--
-- The search is case-sensitive and is performed in the eight usual directions:
--  • left-to-right     ( 1,  0)
--  • right-to-left     (-1,  0)
--  • top-to-bottom     ( 0,  1)
--  • bottom-to-top     ( 0, -1)
--  • diagonal ↘︎       ( 1,  1)
--  • diagonal ↖︎       (-1, -1)
--  • diagonal ↙︎       (-1,  1)
--  • diagonal ↗︎       ( 1, -1)
--
-- All indexes are 1-based so they are easy to read in the test output.
search :: [String]          -- ^ The grid (list of equal-length rows)
       -> [String]          -- ^ Words to look for
       -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, locate w)) wordList
  where
    -- Dimensions of the grid
    nRows = length grid
    nCols = if null grid then 0 else length (head grid)

    -- Safely fetch a character at 1-based coordinates
    charAt :: CharPos -> Maybe Char
    charAt (CharPos c r)
      | r < 1 || r > nRows || c < 1 || c > nCols = Nothing
      | otherwise = Just ((grid !! (r - 1)) !! (c - 1))

    -- All eight search directions (delta-column, delta-row)
    directions :: [(Int, Int)]
    directions = [ ( 1,  0)  -- →
                 , (-1,  0)  -- ←
                 , ( 0,  1)  -- ↓
                 , ( 0, -1)  -- ↑
                 , ( 1,  1)  -- ↘︎
                 , (-1, -1)  -- ↖︎
                 , (-1,  1)  -- ↙︎
                 , ( 1, -1)  -- ↗︎
                 ]

    -- Try to locate a single word
    locate :: String -> Maybe WordPos
    locate [] = Nothing
    locate word@(firstChar : _) =
      -- find the first successful match (if any)
      case [ pos
           | r <- [1 .. nRows]
           , c <- [1 .. nCols]
           , charAt (CharPos c r) == Just firstChar
           , dir <- directions
           , Just pos <- [matchFrom word (CharPos c r) dir]
           ] of
        (p : _) -> Just p
        _       -> Nothing

    -- Check whether the word exists when starting from a given point
    -- and moving in a given direction.
    matchFrom :: String      -- word to match
              -> CharPos     -- starting position (position of first letter)
              -> (Int, Int)  -- direction (delta-col, delta-row)
              -> Maybe WordPos
    matchFrom w (CharPos c0 r0) (dc, dr) =
      let positions = [ CharPos (c0 + dc * k) (r0 + dr * k)
                      | k <- [0 .. length w - 1]
                      ]
       in if all (\(p, ch) -> charAt p == Just ch) (zip positions w)
            then let endPos = last positions
                  in Just (WordPos (CharPos c0 r0) endPos)
            else Nothing
