module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = 
    case filter (/= Nothing) allDirections of
        (Just pos:_) -> Just pos
        _ -> Nothing
  where
    allDirections = 
        [ searchHorizontal grid word
        , searchHorizontalReverse grid word
        , searchVertical grid word
        , searchVerticalReverse grid word
        , searchDiagonalDownRight grid word
        , searchDiagonalDownLeft grid word
        , searchDiagonalUpRight grid word
        , searchDiagonalUpLeft grid word
        ]

-- Search left to right
searchHorizontal :: [String] -> String -> Maybe WordPos
searchHorizontal grid word = searchInLines grid word 0

searchInLines :: [String] -> String -> Int -> Maybe WordPos
searchInLines [] _ _ = Nothing
searchInLines (line:remainingLines) word rowIdx =
    case findSubstring line word 0 of
        Just colIdx -> Just $ WordPos (CharPos (colIdx + 1) (rowIdx + 1)) (CharPos (colIdx + length word) (rowIdx + 1))
        Nothing -> searchInLines remainingLines word (rowIdx + 1)

-- Search right to left
searchHorizontalReverse :: [String] -> String -> Maybe WordPos
searchHorizontalReverse grid word = 
    case searchHorizontal (map reverse grid) (reverse word) of
        Just (WordPos (CharPos startCol startRow) (CharPos endCol endRow)) ->
            let width = length (head grid)
            in Just $ WordPos (CharPos (width - endCol + 1) startRow) (CharPos (width - startCol + 1) endRow)
        Nothing -> Nothing

-- Search top to bottom
searchVertical :: [String] -> String -> Maybe WordPos
searchVertical grid word = 
    case searchHorizontal (transpose grid) word of
        Just (WordPos (CharPos startCol startRow) (CharPos endCol endRow)) ->
            Just $ WordPos (CharPos startRow startCol) (CharPos endRow endCol)
        Nothing -> Nothing

-- Search bottom to top
searchVerticalReverse :: [String] -> String -> Maybe WordPos
searchVerticalReverse grid word = 
    case searchHorizontal (transpose grid) (reverse word) of
        Just (WordPos (CharPos startCol startRow) (CharPos endCol endRow)) ->
            Just $ WordPos (CharPos startRow endCol) (CharPos endRow startCol)
        Nothing -> Nothing

-- Search diagonal down-right
searchDiagonalDownRight :: [String] -> String -> Maybe WordPos
searchDiagonalDownRight grid word = searchDiagonal grid word 1 1

-- Search diagonal down-left
searchDiagonalDownLeft :: [String] -> String -> Maybe WordPos
searchDiagonalDownLeft grid word = searchDiagonal grid word 1 (-1)

-- Search diagonal up-right
searchDiagonalUpRight :: [String] -> String -> Maybe WordPos
searchDiagonalUpRight grid word = searchDiagonal grid word (-1) 1

-- Search diagonal up-left
searchDiagonalUpLeft :: [String] -> String -> Maybe WordPos
searchDiagonalUpLeft grid word = searchDiagonal grid word (-1) (-1)

searchDiagonal :: [String] -> String -> Int -> Int -> Maybe WordPos
searchDiagonal grid word rowDir colDir = 
    let height = length grid
        width = if null grid then 0 else length (head grid)
        positions = [(r, c) | r <- [0..height-1], c <- [0..width-1]]
    in findFirstMatch positions
  where
    findFirstMatch [] = Nothing
    findFirstMatch ((r, c):rest) =
        case matchWordAt grid word r c rowDir colDir of
            Just endPos -> Just $ WordPos (CharPos (c + 1) (r + 1)) endPos
            Nothing -> findFirstMatch rest

matchWordAt :: [String] -> String -> Int -> Int -> Int -> Int -> Maybe CharPos
matchWordAt grid word startRow startCol rowDir colDir =
    if matchesWord grid word startRow startCol rowDir colDir 0
    then Just $ CharPos (startCol + (length word - 1) * colDir + 1) (startRow + (length word - 1) * rowDir + 1)
    else Nothing

matchesWord :: [String] -> String -> Int -> Int -> Int -> Int -> Int -> Bool
matchesWord _ [] _ _ _ _ _ = True
matchesWord grid (w:ws) r c rowDir colDir idx =
    if r >= 0 && r < length grid && c >= 0 && c < length (grid !! r) && (grid !! r) !! c == w
    then matchesWord grid ws (r + rowDir) (c + colDir) rowDir colDir (idx + 1)
    else False

findSubstring :: String -> String -> Int -> Maybe Int
findSubstring [] _ _ = Nothing
findSubstring haystack needle startIdx
    | length haystack < length needle = Nothing
    | take (length needle) haystack == needle = Just startIdx
    | otherwise = findSubstring (tail haystack) needle (startIdx + 1)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)
