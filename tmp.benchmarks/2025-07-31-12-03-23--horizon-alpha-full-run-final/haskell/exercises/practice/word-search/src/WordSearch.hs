module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord grid w)) wordList

-- Internal helpers

-- All 8 directions: E, W, S, N, SE, NW, SW, NE
directions :: [(Int, Int)]
directions =
  [ (1, 0)   -- East
  , (-1, 0)  -- West
  , (0, 1)   -- South
  , (0, -1)  -- North
  , (1, 1)   -- SouthEast
  , (-1, -1) -- NorthWest
  , (1, -1)  -- NorthEast
  , (-1, 1)  -- SouthWest
  ]

-- Try to find the word in the grid; return the first match encountered by
-- scanning rows top-to-bottom, columns left-to-right, and directions in the
-- order defined in `directions`.
findWord :: [String] -> String -> Maybe WordPos
findWord grid word
  | null word = Nothing
  | otherwise = firstJust [ tryFrom r c | r <- [0..h-1], c <- [0..w-1] ]
  where
    h = length grid
    w = if h == 0 then 0 else length (head grid)

    tryFrom :: Int -> Int -> Maybe WordPos
    tryFrom r c =
      firstJust [ matchAtDir r c dx dy | (dx, dy) <- directions ]

    matchAtDir :: Int -> Int -> Int -> Int -> Maybe WordPos
    matchAtDir r c dx dy
      | fits r c dx dy = if allCharsMatch then Just wp else Nothing
      | otherwise = Nothing
      where
        len = length word
        endR = r + (len - 1) * dy
        endC = c + (len - 1) * dx

        fits :: Int -> Int -> Int -> Int -> Bool
        fits r0 c0 ddx ddy =
          r0 >= 0 && r0 < h && c0 >= 0 && c0 < w &&
          endR >= 0 && endR < h && endC >= 0 && endC < w

        charAt :: Int -> Int -> Char
        charAt rr cc = (grid !! rr) !! cc

        allCharsMatch :: Bool
        allCharsMatch =
          and [ charAt (r + i * dy) (c + i * dx) == word !! i
              | i <- [0 .. len - 1]
              ]

        wp = WordPos
              (CharPos { col = c + 1, row = r + 1 })
              (CharPos { col = endC + 1, row = endR + 1 })

-- Helper: return the first Just value from a list, or Nothing
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
  Just _ -> x
  Nothing -> firstJust xs
