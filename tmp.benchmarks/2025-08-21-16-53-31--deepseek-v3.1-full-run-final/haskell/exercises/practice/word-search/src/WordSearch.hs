module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (findWord grid) wordList

findWord :: [String] -> String -> (String, Maybe WordPos)
findWord grid word = (word, searchInAllDirections grid word)

searchInAllDirections :: [String] -> String -> Maybe WordPos
searchInAllDirections grid word = 
    let rows = length grid
        cols = if null grid then 0 else length (head grid)
        positions = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
    in foldl (\acc pos -> case acc of
            Just wp -> acc
            Nothing -> searchFromPosition grid word pos) Nothing positions

searchFromPosition :: [String] -> String -> (Int, Int) -> Maybe WordPos
searchFromPosition grid word (r, c) = 
    let directions = [(0,1), (1,0), (0,-1), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]
    in foldl (\acc dir -> case acc of
            Just wp -> acc
            Nothing -> searchInDirection grid word (r, c) dir) Nothing directions

searchInDirection :: [String] -> String -> (Int, Int) -> (Int, Int) -> Maybe WordPos
searchInDirection grid word (r, c) (dr, dc) = 
    let rows = length grid
        cols = if null grid then 0 else length (head grid)
        positions = take (length word) $ iterate (\(row, col) -> (row + dr, col + dc)) (r, c)
    in if all (\(row, col) -> row >= 0 && row < rows && col >= 0 && col < cols) positions
        then let chars = map (\(row, col) -> (grid !! row) !! col) positions
             in if chars == word
                then let startPos = CharPos {col = c + 1, row = r + 1}
                         endPos = CharPos {col = c + (dc * (length word - 1)) + 1, 
                                          row = r + (dr * (length word - 1)) + 1}
                     in Just $ WordPos {start = startPos, end = endPos}
                else Nothing
        else Nothing
