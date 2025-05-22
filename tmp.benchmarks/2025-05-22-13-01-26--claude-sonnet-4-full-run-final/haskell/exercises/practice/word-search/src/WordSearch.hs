module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = 
    let positions = [(r, c) | r <- [0..length grid - 1], c <- [0..length (head grid) - 1]]
        directions = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]
        results = [pos | (r, c) <- positions, (dr, dc) <- directions, 
                   let pos = searchDirection grid word r c dr dc, 
                   case pos of Just _ -> True; Nothing -> False]
    in case results of
        (result : _) -> result
        _ -> Nothing

searchDirection :: [String] -> String -> Int -> Int -> Int -> Int -> Maybe WordPos
searchDirection grid word startRow startCol deltaRow deltaCol
    | not (isValidPosition grid startRow startCol) = Nothing
    | null word = Nothing
    | otherwise = 
        let endRow = startRow + (length word - 1) * deltaRow
            endCol = startCol + (length word - 1) * deltaCol
        in if isValidPosition grid endRow endCol && 
              matchesWord grid word startRow startCol deltaRow deltaCol
           then Just $ WordPos (CharPos startCol startRow) (CharPos endCol endRow)
           else Nothing

isValidPosition :: [String] -> Int -> Int -> Bool
isValidPosition grid rowPos colPos = 
    rowPos >= 0 && rowPos < length grid && 
    colPos >= 0 && colPos < length (head grid)

matchesWord :: [String] -> String -> Int -> Int -> Int -> Int -> Bool
matchesWord grid word rowPos colPos deltaRow deltaCol = 
    let positions = take (length word) [(rowPos + i * deltaRow, colPos + i * deltaCol) | i <- [0..]]
        chars = [grid !! r !! c | (r, c) <- positions, isValidPosition grid r c]
    in length chars == length word && chars == word
