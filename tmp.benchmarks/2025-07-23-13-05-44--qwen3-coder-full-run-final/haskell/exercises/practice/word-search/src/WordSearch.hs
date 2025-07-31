module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, findWord grid word) | word <- wordList]

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = findWordInGrid (zip grid [0..]) word

findWordInGrid :: [(String, Int)] -> String -> Maybe WordPos
findWordInGrid gridRows word = 
  findInAllDirections gridRows word [(0,1),(1,0),(0,-1),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]

findInAllDirections :: [(String, Int)] -> String -> [(Int, Int)] -> Maybe WordPos
findInAllDirections gridRows word [] = Nothing
findInAllDirections gridRows word (dir:dirs) =
  case findInDirection gridRows word dir of
    Nothing -> findInAllDirections gridRows word dirs
    Just pos -> Just pos

findInDirection :: [(String, Int)] -> String -> (Int, Int) -> Maybe WordPos
findInDirection gridRows word (dx, dy) = 
  findAtPosition gridRows word (dx, dy) 0 0
  where
    height = length gridRows
    width = if height > 0 then length (fst (head gridRows)) else 0
    
    findAtPosition :: [(String, Int)] -> String -> (Int, Int) -> Int -> Int -> Maybe WordPos
    findAtPosition rows w (dx', dy') x y
      | y >= height = Nothing
      | x >= width = findAtPosition rows w (dx', dy') 0 (y + 1)
      | otherwise = 
          case checkWordAtPosition rows w (dx', dy') x y of
            Nothing -> findAtPosition rows w (dx', dy') (x + 1) y
            Just pos -> Just pos

checkWordAtPosition :: [(String, Int)] -> String -> (Int, Int) -> Int -> Int -> Maybe WordPos
checkWordAtPosition gridRows word (dx, dy) startX startY = 
  if canPlaceWord gridRows word (dx, dy) startX startY
    then Just $ WordPos 
           (CharPos (startX + 1) (startY + 1)) 
           (CharPos (startX + dx * (length word - 1) + 1) (startY + dy * (length word - 1) + 1))
    else Nothing

canPlaceWord :: [(String, Int)] -> String -> (Int, Int) -> Int -> Int -> Bool
canPlaceWord gridRows word (dx, dy) startX startY = 
  all (uncurry checkChar) (zip word [0..(length word - 1)])
  where
    checkChar c i = 
      let newX = startX + i * dx
          newY = startY + i * dy
      in validPosition newX newY && getCharAt newX newY == c
    
    validPosition x y = 
      x >= 0 && x < width && y >= 0 && y < height
    
    height = length gridRows
    width = if height > 0 then length (fst (head gridRows)) else 0
    
    getCharAt x y = (fst (gridRows !! y)) !! x
