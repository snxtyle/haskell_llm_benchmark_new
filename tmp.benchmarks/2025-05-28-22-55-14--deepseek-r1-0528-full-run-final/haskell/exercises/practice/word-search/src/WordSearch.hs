module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList =
  map (\word -> (word, findWord word)) wordList
  where
    height = length grid
    width = if height == 0 then 0 else length (head grid)
    directions = [(dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]

    inbounds :: CharPos -> Bool
    inbounds (CharPos x y) = x >= 0 && x < width && y >= 0 && y < height

    charAt :: CharPos -> Char
    charAt (CharPos x y) = (grid !! y) !! x

    move :: (Int, Int) -> CharPos -> CharPos
    move (dx, dy) (CharPos x y) = CharPos (x+dx) (y+dy)

    moveMany :: Int -> (Int, Int) -> CharPos -> CharPos
    moveMany k (dx, dy) (CharPos x y) = CharPos (x + k*dx) (y + k*dy)

    tryDirection :: String -> CharPos -> (Int, Int) -> Bool
    tryDirection [] _ _ = True
    tryDirection (c:cs) pos dir =
        if not (inbounds pos)
        then False
        else
            if charAt pos == c
            then tryDirection cs (move dir pos) dir
            else False

    findWord :: String -> Maybe WordPos
    findWord word
      | null word = Nothing
      | otherwise =
        let starts = [ CharPos x y | y <- [0..height-1], x <- [0..width-1] ]
            n = length word
            matches = [ (startPos, direc) | startPos <- starts, direc <- directions, tryDirection word startPos direc ]
        in case matches of
             [] -> Nothing
             (match:_) -> 
                 let (startPos, direc) = match
                     endPos = moveMany (n-1) direc startPos
                     -- Convert 0-indexed to 1-indexed coordinates
                     toIndexed (CharPos x y) = CharPos (x+1) (y+1)
                 in Just (WordPos (toIndexed startPos) (toIndexed endPos))
