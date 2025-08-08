module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (searchWord grid) wordList

searchWord :: [String] -> String -> (String, Maybe WordPos)
searchWord grid word = (word, findWord grid word)

findWord :: [String] -> String -> Maybe WordPos
findWord grid word
  | null grid || null word = Nothing
  | otherwise = findFirstMatch positions
  where
    height = length grid
    width = if height > 0 then length (head grid) else 0
    positions = [checkDirection grid word row col dr dc |
                 row <- [0..height-1],
                 col <- [0..width-1],
                 dr <- [-1, 0, 1],
                 dc <- [-1, 0, 1],
                 not (dr == 0 && dc == 0)]
    findFirstMatch [] = Nothing
    findFirstMatch (Just pos:_) = Just pos
    findFirstMatch (Nothing:rest) = findFirstMatch rest

checkDirection :: [String] -> String -> Int -> Int -> Int -> Int -> Maybe WordPos
checkDirection grid word startRow startCol deltaRow deltaCol
  | matchesWord grid word startRow startCol deltaRow deltaCol =
      Just WordPos { start = CharPos { col = startCol, row = startRow },
                     end = CharPos { col = endCol, row = endRow } }
  | otherwise = Nothing
  where
    wordLen = length word
    endRow = startRow + deltaRow * (wordLen - 1)
    endCol = startCol + deltaCol * (wordLen - 1)

matchesWord :: [String] -> String -> Int -> Int -> Int -> Int -> Bool
matchesWord grid word startRow startCol deltaRow deltaCol =
    all matchesAt (zip [0..] word)
  where
    matchesAt (i, expectedChar) =
      let row = startRow + deltaRow * i
          col = startCol + deltaCol * i
      in isValidPosition grid row col && getCharAt grid row col == expectedChar

isValidPosition :: [String] -> Int -> Int -> Bool
isValidPosition grid row col =
    row >= 0 && row < length grid &&
    col >= 0 && col < length (grid !! row)

getCharAt :: [String] -> Int -> Int -> Char
getCharAt grid row col = (grid !! row) !! col
