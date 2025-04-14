module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord w grid)) wordList

-- Find a word in the grid, returning the position if found
findWord :: String -> [String] -> Maybe WordPos
findWord word grid = 
    let rows = length grid
        cols = if null grid then 0 else length (head grid)
        directions = [(dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
    in searchAllPositions word grid rows cols directions

-- Search all positions and directions for the word
searchAllPositions :: String -> [String] -> Int -> Int -> [(Int, Int)] -> Maybe WordPos
searchAllPositions word grid rows cols directions = 
    let positions = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
    in foldr (\(r,c) acc -> case acc of 
        Just pos -> Just pos
        Nothing -> searchDirections word grid r c directions) Nothing positions

-- Search all directions from a starting position for the word
searchDirections :: String -> [String] -> Int -> Int -> [(Int, Int)] -> Maybe WordPos
searchDirections word grid startRow startCol directions = 
    foldr (\dir acc -> case acc of 
        Just pos -> Just pos
        Nothing -> checkDirection word grid startRow startCol dir) Nothing directions

-- Check if a word exists in a specific direction from starting position
checkDirection :: String -> [String] -> Int -> Int -> (Int, Int) -> Maybe WordPos
checkDirection word grid startRow startCol (dx, dy) = 
    let positions = take (length word) [(startRow + i*dy, startCol + i*dx) | i <- [0..]]
        endRow = startRow + (length word - 1) * dy
        endCol = startCol + (length word - 1) * dx
    in if all (\(r,c) -> r >= 0 && r < length grid && c >= 0 && c < length (head grid)) positions
       then let chars = map (\(r,c) -> (grid !! r) !! c) positions
                foundWord = foldr (\c acc -> c:acc) "" chars
            in if foundWord == word
               then Just $ WordPos (CharPos (startCol + 1) (startRow + 1)) (CharPos (endCol + 1) (endRow + 1))
               else Nothing
       else Nothing
