module WordSearch
  ( search
  , CharPos (..)
  , WordPos (..) )
where

import Data.Char  (toLower)
import Data.List  (find)
import Data.Maybe (listToMaybe)

----------------------------------------------------------------------
-- Data types supplied by the exercise
----------------------------------------------------------------------

data CharPos = CharPos
  { col :: Int  -- ^ 1‑based column number
  , row :: Int  -- ^ 1‑based row number
  } deriving (Eq, Show)

data WordPos = WordPos
  { start :: CharPos  -- ^ position of the first letter
  , end   :: CharPos  -- ^ position of the last  letter
  } deriving (Eq, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

-- | Given a puzzle grid and a list of words, return the location of
--   the first and last letter for each word.  If a word does not
--   occur in the grid, the corresponding position is 'Nothing'.
--
--   The search is case‑insensitive and proceeds in eight directions:
--     • horizontally   (left→right, right→left)
--     • vertically     (top→bottom, bottom→top)
--     • diagonally     (four possible directions)
--
--   The coordinates that are returned are 1‑based, i.e. the top‑left
--   corner of the puzzle has position (1,1).
search :: [String]            -- ^ puzzle grid (all rows must be equal length)
       -> [String]            -- ^ list of words to look for
       -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, locateWord w)) wordList
  where
    -- Pre‑compute some helpers about the grid
    nRows = length grid
    nCols = if null grid then 0 else length (head grid)

    -- Lower‑cased character at a given position (1‑based indices).
    -- Returns Nothing when outside the grid boundaries.
    charAt :: Int -> Int -> Maybe Char
    charAt x y
      | x < 1 || x > nCols = Nothing
      | y < 1 || y > nRows = Nothing
      | otherwise          = Just . toLower $ (grid !! (y - 1)) !! (x - 1)

    -- All eight possible directions expressed as (dx,dy) increments
    directions :: [(Int, Int)]
    directions =
      [ ( 1,  0)  -- →
      , ( 0,  1)  -- ↓
      , ( 1,  1)  -- ↘
      , ( 1, -1)  -- ↗
      , (-1,  0)  -- ←
      , ( 0, -1)  -- ↑
      , (-1, -1)  -- ↖
      , (-1,  1)  -- ↙
      ]

    -- Locate a single word inside the grid
    locateWord :: String -> Maybe WordPos
    locateWord w =
      listToMaybe
        [ WordPos (CharPos sx sy) (CharPos ex ey)
        | sy <- [1 .. nRows]
        , sx <- [1 .. nCols]
        , Just c0 <- [charAt sx sy]           -- letter at the start position
        , toLower (head w) == c0              -- first letter matches
        , (dx, dy) <- directions
        , let (ex, ey) = (sx + (len - 1) * dx, sy + (len - 1) * dy)
        , inBounds ex ey                      -- word fits in the grid
        , matchAlongDirection sx sy dx dy     -- and all letters match
        ]
      where
        len = length w
        lowerWord = map toLower w

        inBounds x y = x >= 1 && x <= nCols && y >= 1 && y <= nRows

        -- Check if the word matches starting at (sx,sy) moving (dx,dy)
        matchAlongDirection :: Int -> Int -> Int -> Int -> Bool
        matchAlongDirection sx sy dx dy =
          and
            [ maybe False (== expected) (charAt (sx + i * dx) (sy + i * dy))
            | (i, expected) <- zip [0 ..] lowerWord
            ]
