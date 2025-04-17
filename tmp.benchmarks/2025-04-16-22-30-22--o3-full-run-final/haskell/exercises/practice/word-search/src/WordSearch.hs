module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)
import Data.List  (find)

-- | 1‑based coordinates of a single character
data CharPos = CharPos { col :: Int  -- ^ column  (1 = first column, increases to the right)
                       , row :: Int  -- ^ row     (1 = first row,    increases downwards)
                       } deriving (Eq, Show)

-- | Start and end coordinates of a word (inclusive)
data WordPos = WordPos { start :: CharPos
                       , end   :: CharPos
                       } deriving (Eq, Show)

-- | Find every word from the list inside the given grid.
--   Words may appear horizontally, vertically and diagonally,
--   in both forward and reverse directions.
--   Each result contains the word together with the coordinates
--   of its first and last character, if it is found.
search :: [String]            -- ^ puzzle grid (each element is a row)
       -> [String]            -- ^ list of words to search for
       -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, locate w)) wordList
  where
    height = length grid
    width  = if null grid then 0 else length (head grid)

    -- All eight possible directions (dx, dy)
    directions :: [(Int, Int)]
    directions =
      [ ( 1,  0)  -- →  right
      , (-1,  0)  -- ←  left
      , ( 0,  1)  -- ↓  down
      , ( 0, -1)  -- ↑  up
      , ( 1,  1)  -- ↘  down‑right
      , (-1, -1)  -- ↖  up‑left
      , ( 1, -1)  -- ↗  up‑right
      , (-1,  1)  -- ↙  down‑left
      ]

    -- Safe character lookup: Nothing when indices are out of bounds
    charAt :: Int -> Int -> Maybe Char
    charAt x y
      | x < 1 || x > width  = Nothing
      | y < 1 || y > height = Nothing
      | otherwise           = Just $ (grid !! (y - 1)) !! (x - 1)

    -- Attempt to find the given word in the grid; return its position if found
    locate :: String -> Maybe WordPos
    locate w = listToMaybe $ do
      -- enumerate every cell in reading order
      y <- [1 .. height]
      x <- [1 .. width]
      dir@(dx, dy) <- directions
      -- prune early if the first letter does not match
      guard (charAt x y == Just firstLetter)
      let positions = take wordLen $ iterate (\(cx, cy) -> (cx + dx, cy + dy)) (x, y)
      guard (length positions == wordLen)                   -- always true, but keeps 'positions' in scope
      guard (all matchLetter (zip positions w))
      let (ex, ey) = last positions
      pure WordPos { start = CharPos x y
                   , end   = CharPos ex ey
                   }
      where
        firstLetter = head w
        wordLen     = length w

        matchLetter :: ((Int, Int), Char) -> Bool
        matchLetter ((cx, cy), ch) = charAt cx cy == Just ch

    -- Simple replacement for Control.Monad.guard from base <4.8
    guard :: Bool -> [()]
    guard True  = [()]
    guard False = []
